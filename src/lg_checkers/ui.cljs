(ns lg-checkers.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [lg-checkers.board :refer [board board-events]]))

(enable-console-print!)

; == UI events ==========================================
(defn board-click [board-pos]
  "when we click a game square, we send an event"
  (put! board-events {:event :board-clicked :position board-pos})
)

; == Board UI Drawing ===================================
(defn draw-piece [piece-type]
  "draw pieces based on the piece-type"
  (apply dom/div #js {:className piece-type} nil))

(defn draw-tuple [piece row-odd?]
  "draws pairs of checkerboard squares within a row
   depending on if row is odd or even."
	(let [piece-type (name (last piece))
		    piece-pos (first piece)
        white-square (dom/td #js {:className "white"})
        green-square (dom/td #js {:className "green"
                                  :onClick
                                    (fn [e] (board-click
                                             piece-pos))}
                                 (draw-piece piece-type))]
    (if row-odd?
      [white-square green-square]
      [green-square white-square])))

(defn draw-row [row]
  "given a row, determine if it is an odd or even row
   and iterates over the board positions, drawing each
   tuple of checkerboard squares"
  (let [curr-row (/ (first (last row)) 4)
        row-odd? (odd? curr-row)]
    (apply dom/tr nil
      (mapcat #(draw-tuple % row-odd?)
           row))))

(defn console [text owner]
  "consoler"
  (om/component
   (dom/div #js {:className "console"}
     (dom/p nil text))))

(defn checkerboard [board owner]
  "given a checkerboard data structure, partition into
   rows and draw the individual rows"
   (om/component
    (dom/div #js {:id "checkerboard-wrapper"}
      (dom/h2 nil "Checkers board game")
        (dom/div nil
          (apply dom/table #js {:className "checkerboard"}
            (map draw-row
              (partition 4 board)))
          (om/build console "Start the game by making a move")))))

; == Bootstrap ============================================
(defn bootstrap-ui []
  (om/root checkerboard board
       {:target (. js/document (getElementById "checkers"))}))

(bootstrap-ui)
