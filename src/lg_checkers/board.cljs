(ns lg-checkers.board
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [datascript :as ds] [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

; == Notes ==============================================
; Board pieces are defined in the checkers.css file.  The
; currently defined pieces are:
;     :red-piece
;     :black-piece
;     :prom-red-piece
;     :prom-black-piece
;     :empty
;
; The board is laid out as a 32 element map, one element
; for each position.  It is stored in an atom, and bound
; to the UI.  Any update of the atom will cause an UI
; refresh to reflect the current board state.
;
; core.async is used to implement CSP (Communicating
; Sequential Proceses), and channels are used to report
; user interaction events, as well as changing the board
; state.

; ===Channels ===========================================
; the board generates events on this channel
;     {:event :event-symbol
;      :position <int>}
(defonce board-events (chan))

; the board receives commands to manipulate its state
;     {:command :command-symbol
;      :position <integer>
;      :piece :piece-symbol}
(defonce board-commands (chan))

; ai events on this channel
;     {:event :event-symbol }
(defonce ai-events (chan))

; the ai receives commands to manipulate its state
;     {:command :command-symbol}
(defonce ai-commands (chan))

; console-events
;     {:event :event-symbol }
(defonce console-events (chan))

; console-commands
;     {:command :command-symbol}
(defonce console-commands (chan))

; == Board State ==========================================
(defn create-board []
  "initialize a board, where positions are indexed 1-32.
   each position contains the symbol of the piece in it."
  (atom
   (apply sorted-map
          (flatten
           (map-indexed (fn [i v] (vector (inc i) v))
                        (flatten
                         [(repeat 12 :red-piece)
                          (repeat 8 :empty-piece)
                          (repeat 12 :black-piece)]))))))

; DATASCRIPT
;
;
(def conn (ds/create-conn {}))

(defonce initial-state {:board (create-board)
                        :chat (atom [{:by "Computer"
                                      :text "Challenge me I'm waiting for you to go..."}])})
(defonce chat (:chat initial-state))
(defonce board (:board initial-state))
(defonce turn (atom {:user true}))
(defonce playing (atom {:playing true}))
(defonce ai :aggresive) ; choose retarded / aggresive / passive / solved
(defonce computer (atom []))

; board constants
(def num-rows 8)
(def row-width 4)
(def top-row 1)
(def bottom-row 8)

; === Utility Functions =================================

; [NOTE:] Challengee should investigate memoization of this function. (clojure makes this quite simple :P yay for caching)
(defn compute-pos-neighbors-unmem
  "Given a position on a 1-indexed checkerboard, return a list of immediate
   diagonal neighbors"
  [pos]
  (let [current-row         (int (Math/ceil (/ pos row-width))) ; kept getting floats for some reason
        ; calculate boolean conditional for each board edge
        top-edge?           (= current-row top-row)
        bottom-edge?        (= current-row bottom-row)
        left-edge?          (= (+ row-width 1) (mod pos num-rows))
        right-edge?         (= row-width (mod pos num-rows))
        ; build a map for each cardinal direction based on if we're odd or even
        dirs                (if (odd? current-row)
                               ; relative positions for odd rows
                               {:nw (- pos row-width)
                                :ne (- pos (- row-width 1))
                                :sw (+ pos row-width)
                                :se (+ pos (+ row-width 1))}
                               ; relative positions for even rows
                               {:nw (- pos (+ row-width 1))
                                :ne (- pos row-width)
                                :sw (+ pos (- row-width 1))
                                :se (+ pos row-width)})
        ; determine potential neighbors for each cardinal direction
        potential-neighbors {:nw (or top-edge? left-edge?     (:nw dirs))
                             :ne (or top-edge? right-edge?    (:ne dirs))
                             :sw (or bottom-edge? left-edge?  (:sw dirs))
                             :se (or bottom-edge? right-edge? (:se dirs))}]
    ; finally, filter for directions that only have numerics and no booleans
    (filter #(number? %) (vals potential-neighbors))))

(def compute-pos-neighbors
  "Memoized version of neighbor computation"
  (memoize compute-pos-neighbors-unmem))

(defn compute-neighbor-positions []
  "Compute neighbors for every board position"
  (map (fn [pos] {pos (compute-pos-neighbors pos)})
       (range 1 33)))

;(println @board) ; board-init

; === Game Play Functions =================================
;
(defn in-neighborhood? [neighborhood value]
  "Check to see if one position is a possible neighbor of another"
  (some #(= value %) neighborhood))

; Example for console for bottom scenario
;(println (compute-pos-neighbors 15)) ; for ref - map of board
;(println (compute-pos-neighbors 8))

(defn determine-jumped-checker [at to]
  "Use the shared neighborhood value to determine if a jump is valid/possible -
  Ex. Here is a possible black-piece jump from pos 15 to 8 with a red-piece at 11
      (compute-pos-neighbors 15) -> (10 11 18 19)
      (compute-pos-neighbors 8) ->  (3 4 11 12)
      The neighorhoods of the checkers share (11 - red-piece) so I can infer its at least a valid jump
      this holds true for the converse aswell i.e if no shared value exists it must not be valid"
  (some (set (compute-pos-neighbors at)) (compute-pos-neighbors to)))

(defn determine-move-type [at to]
  "Determine the move type"
  (cond
    (in-neighborhood? (compute-pos-neighbors at) to) :basic-move
    :else :jump-move))

(defn checker-row [postition]
  "Map checker position to row in board"
  (int (Math/ceil (/ postition row-width)))) ; thx utility function for saving some thinking time :P

(defn cardinality-direction-unmem [at to]
  "Determine the cardinal direction based on an at and to"
  (let [position-diff (- to at)
        moving-up? (> 0 position-diff)
        at-row (checker-row at)
        odd-row? (odd? at-row)
        even-row? (even? at-row)]
    (if moving-up?
      (cond
        (= position-diff -8) :n
        (= position-diff -1) :w
        (= position-diff 1)  :e
        (or (and odd-row? (= position-diff row-width))
            (and even-row? (not= position-diff row-width))) :nw
        :else :ne)
      (cond
         (= position-diff 8)  :s
         (= position-diff -1) :w
         (= position-diff 1)  :e
         (or (and odd-row? (= position-diff row-width))
             (and even-row? (not= position-diff row-width))) :se
         :else :sw))))

(def cardinality-direction
  "Memoized version of cardinality direction computation"
  (memoize cardinality-direction-unmem))

(defn piece-cardinality-direction [piece]
  "Determine the allowable cardinal direction"
  (case piece
    :black-piece #{:nw :ne}
    :red-piece #{:sw :se}
    :prom-black-piece #{:ne :nw :se :sw}
    :prom-red-piece #{:ne :nw :se :sw}))

(defn valid-cardinality? [at to piece-type]
  "Return if a cardinality direction is in the allowable set based on the piece type"
  (not
    (nil?
      ((cardinality-direction at to)
      (piece-cardinality-direction piece-type)))))

(defn movement-allowed? [at to move-type piece-type]
  "Determine if the movement spot is valid based on move positions and type"
  (and
    (valid-cardinality? at to piece-type)
    (if (= move-type :jump-move)
      (let [jumped-checker-pos (determine-jumped-checker at to)
            jumped-checker-type (checker-at jumped-checker-pos)]
        (and
          (valid-cardinality? jumped-checker-pos to piece-type)
          (and
            (not (empty-position? jumped-checker-pos))
            (and (not= piece-type jumped-checker-type)
                 (not= (get-king-of piece-type) jumped-checker-type)
                 (not= (get-not-king-of piece-type) jumped-checker-type))))) ; this is sloppy/ugly conditioning...will think of better solution with refactor
            true))) ; if its not a jump move it defaults to (1st expr && true)

(def movement-allowed?-mem
  "Memoized version of movement-allowed? computation"
  (memoize movement-allowed?))

(defn diff-position? [position-one position-two]
  (not= position-one position-two))

(defn checker-at [position]
  "Get checker metadata at the board position"
  (get @board position))

(defn empty-position? [position]
  "Determine if the position is empty on the board"
  (= :empty-piece (checker-at position)))

(defn king-it? [row-position piece-type]
  "Determine if the the piece should be a fucking king based on row"
  (case piece-type
    :red-piece (= row-position 8)
    :black-piece (= row-position 1)
    false))

(defn get-king-of [piece-type]
  "Get the correct king piece based on type"
    (case piece-type
      :red-piece :prom-red-piece
      :black-piece :prom-black-piece
      false))

(defn get-not-king-of [piece-type]
  "Get the correct non-king piece based on type"
    (case piece-type
      :prom-red-piece :red-piece
      :prom-black-piece :black-piece
      false))

(defn valid-move? [at to move-type piece-type]
  "Determine if a players move is valid"
    (and
     (diff-position? at to)                            ; different positions clicked
     (not (empty-position? at))                        ; first click was on a checker
     (empty-position? to)                              ; second click was on an empty space
     (movement-allowed? at to move-type piece-type)    ; valid landing by move and piece type
    ))

(def valid-move?-mem
  "Memoized version of valid-move? computation"
  (memoize valid-move?))

(defn async-some [predicate input-chan]
  "Tests some condition against an event channel notification to see if we should store or ignore
  i.e This handles the situation for clicks on empty cells or its not a users turn"
  (go-loop []
        (let [ev (<! input-chan)]
          (if (predicate ev)
            ev
        (recur)))))

(defn process-board-click [handler input-chan]
  "Helper for the board event CP to handle invalid clicks"
  (async-some handler input-chan))

(defn process-ai-computation [ai-break input-chan]
  "Helper for the ai event CP to handle computation time"
  (async-some ai-break input-chan))

(defn is-users-turn? [_]
  "Determine if its the users turn based on the turn atom"
  (get @turn :user))

(defn is-black? [value]
  "Is a red piece"
  (or (= value :black-piece) (= value :prom-black-piece)))

(defn is-red? [value]
  "Is a red piece"
  (or (= value :red-piece) (= value :prom-red-piece)))

(defn filter-board-by-pred [fn]
  (into {} (filter (comp fn val) @board)))

(defn get-black-pieces [_]
  "Get the black pieces in the board"
  (filter-board-by-pred is-black?))

(defn get-red-pieces [_]
  "Get the red pieces in the board"
  (filter-board-by-pred is-red?))

(defn game-over? [_]
  "Determine if the game is over!"
  (cond
    (= 0 (count (get-red-pieces)))
         (put! board-commands {:command :game-over :winner "Black"})
    (= 0 (count (get-black-pieces)))
         (put! board-commands {:command :game-over :winner "Red"})))

(defn has-some-jump [value neighborhood piece-type]
  (let [extended-neighborhood (distinct (reduce concat (map compute-pos-neighbors neighborhood)))]
  (some #(when (valid-move? value % :jump-move piece-type) %) extended-neighborhood)))

(defn has-some-basic [value neighborhood piece-type]
  (some #(when (valid-move? value % :basic-move piece-type) %) neighborhood))

(defn determine-piece-type [determining-function piece-type]
  "Determine a piece type based on board position ( king or not king )"
  (let [king? (king-it? (checker-row determining-function) piece-type)]
    (cond
     king? (get-king-of piece-type)
     :else piece-type)))

; === AI Functions =================================
;
;
; determine the best possible move through minimax algorithm
(defn move-helper [determined at to piece-type move-type]
  "Return move helper map"
  {:determined determined
   :at at
   :to to
   :piece-type piece-type
   :move-type move-type})

; Computer talker that accepts a move function and chats his life away
;
(defn computer-talker [move-function]
  "Take current board / Choose a random red-piece / Determine a basic neighbor move or jump if avail"
  (let [_ (println "------- COMPUTER TURN -------")
        _ (println @board)
        _ (println "@ DETERMINING MOVE BASED ON YOUR AWFUL MOVE...")
        remaining-pieces (keys (get-red-pieces))
        _ (println (str "@@ HMMMM I HAVE...") (count remaining-pieces) " pieces left")
        _ (println (str "@@ MY PIECES ARE AT POSITIONS..." remaining-pieces))
        computer-move-selected (move-function remaining-pieces)
        at (get computer-move-selected :at)
        piece-type (get computer-move-selected :piece-type)
        _ (println (str "@@ SELECTED MY " piece-type " TO CRUSH YOU WITH...."))
        to (get computer-move-selected :to)
        move (get computer-move-selected :move-type)
        _ (println "@@@ DETERMINED THE BEST MOVE TO CRUSH YOU....")
        _ (println (str "@@@@ AHA! I WILL CRUSH YOU --- " "moved from " at " " to))
        _ (println "------- END COMPUTER TURN -------")]
    (move-helper true at to piece-type move)))

(defn generic-find-move [move-function]
  (let [choose (keys (get-red-pieces))]
    (into {} (map #(-> {(keyword (str %))
                      (move-function %
                       (compute-pos-neighbors %)
                       (checker-at %))}) choose))))

(defn aggresive-computer-move [_]
  "Find all availiable jumps and pick one otherwise just do a basic move."
  (let [jump-checker (filter val (generic-find-move has-some-jump))
        basic-checker (rand-nth (filter val (generic-find-move has-some-basic)))]
    (if (and (not= 0 (count jump-checker)) (< 15 (rand-int 100))) ; adds in randomization for jumping to 90%
      (let [selected-checker (rand-nth jump-checker)
            at (int (name (first selected-checker)))
            to (last selected-checker)
            checker-type (checker-at at)]
        {:at at
         :to to
         :move-type :jump-move
         :piece-type (determine-piece-type to checker-type)})
      (let [at (int (name (first basic-checker)))
            to (last basic-checker)
            checker-type (checker-at at)]
              {:at at
               :to to
               :move-type :basic-move
               :piece-type (determine-piece-type to checker-type)}))))

(defn find-aggresive-computer-move []
  "Aggresive strategy - ALWAYS KILL!"
    (let [aggresive-move (computer-talker aggresive-computer-move)]
      (println aggresive-move)
    (swap! computer conj aggresive-move)
    (put! ai-commands {:command :make-move})))

(defn dumbass-computer-move [keys-to-search]
  "Choose a computer players piece send back position as map. Takes in an excluded set to filter out"
  (let [random-key-at (rand-nth keys-to-search)
        checker-type (checker-at random-key-at)
        its-neighborhood (compute-pos-neighbors random-key-at)
        has-a-jump-move (has-some-jump random-key-at its-neighborhood checker-type) ; jump takes priority
        has-a-basic-move (has-some-basic random-key-at its-neighborhood checker-type)]
      (cond
        has-a-jump-move {:at random-key-at
                         :to has-a-jump-move
                         :move-type :jump-move
                         :piece-type (determine-piece-type has-a-jump-move checker-type)}
        has-a-basic-move {:at random-key-at
                          :to has-a-basic-move
                          :move-type :basic-move
                          :piece-type (determine-piece-type has-a-basic-move checker-type)}
        :else (dumbass-computer-move (remove #(= random-key-at %) keys-to-search)))))

(defn find-dumbass-computer-move []
  "Dumbass strategy"
  (let [dumbass-move (computer-talker dumbass-computer-move)]
    (swap! computer conj dumbass-move)
    (put! ai-commands {:command :make-move})))

(defn find-solved-computer-move []
  "Solved strategy") ; TODO

(defn find-passive-computer-move []
  "Passive strategy - KILL LAST and move from the back where possible") ; TODO

; MINIMAX VERSION FOR SOLVED --- will need to try this later
;(defn minimax [node depth maximizing-player]
 ; (cond
  ; (or (= depth 0) is-terminal-node node))

;(defn is-terminal-node [node]
 ; "Determine if node is a terminal node")

; == Concurrent Processes =================================
; this concurrent process reacts to board click events
(go-loop []
        (let [event-at (<! (process-board-click
                             (fn [event] (and
                                          (is-users-turn?)
                                          (not (empty-position? (:position event)))
                                          (is-black? (checker-at (:position event)))))
                            board-events))
              event-to (<! board-events)
              move-at (:position event-at)
              move-to (:position event-to)
              move-type (determine-move-type move-at move-to)
              piece-type (checker-at move-at)
              users-turn? (is-users-turn?)]
        (cond
         users-turn?
          (let [position-map {:at move-at :to move-to}]
            (when (valid-move? move-at move-to move-type piece-type)
              (put! board-commands
                    {:command :update-board-position
                     :position position-map
                     :piece piece-type
                     :move-type move-type
                     :jumped-checker (when (= move-type :jump-move)
                                       (determine-jumped-checker move-at move-to))})
              (when (king-it? (checker-row move-to) piece-type)
                (put! board-commands
                      {:command :king-it
                       :position move-to
                       :piece (get-king-of piece-type)}))))
          :else (js/alert "Waiting on the computer hes thinking...")))
        (recur))

; this concurrent process receives board command messages and executes on them.
(go-loop []
      (let [command (<! board-commands)
            event (:command command)]
        (case event
          :update-board-position
            (let [position (:position command)
                  piece (:piece command)
                  at (:at position)
                  to (:to position)
                  move-type (:move-type command)
                  jumped-checker (:jumped-checker command)
                  next-turn (not (is-users-turn?))]
            (swap! board assoc at :empty-piece to piece) ; move piece
            (when (not (nil? jumped-checker))
              (swap! board assoc jumped-checker :empty-piece))
            (swap! turn assoc :user next-turn) ; toggle the turn
            (when (not next-turn)
              (put! ai-commands {:command :determine-next-computer-move }))
            (game-over?))
          :king-it
            (let [position (:position command)
                  piece-type (:piece command)]
              (swap! board assoc position piece-type))
          :game-over
            (let [winning-color (:winner command)]
            (js/alert (str "Game is over! " winning-color " wins!")))))
        (recur))

; this concurrent process reacts to ai trigger events
(go-loop []
      (let [command (<! ai-commands)
            event (:command command)]
        (case event
          :make-move
            (let [determined-move (last @computer)
                  determined (get determined-move :determined)
                  to (get determined-move :to)
                  at (get determined-move :at)]
              (cond
                determined (let [move-type (get determined-move :move-type)
                                 piece-type (get determined-move :piece-type)
                                 position-map {:at at :to to}
                                 jumped-checker (when (= move-type :jump-move)
                                                  (determine-jumped-checker at to))]
                            (put! board-commands {:command :update-board-position
                                                  :position position-map
                                                  :piece piece-type
                                                  :move-type move-type
                                                  :jumped-checker jumped-checker}))
               :else (put! ai-events)))

          :determine-next-computer-move
            (case ai
              :retarded (find-dumbass-computer-move)
              :aggresive (find-aggresive-computer-move)
              :passive (find-passive-computer-move)
              :solved (find-solved-computer-move))))
        (recur))

; this concurrent process reacts to ai trigger events
(go-loop []
      (let [command (<! console-commands)
            event (:command command)]
        (case event
          :write (:text command))))
