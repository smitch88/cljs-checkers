(ns lg-checkers.test.stubs
  (:use clojure.test))

; ===Function Units ===========================================
(defn diff-position? [x y] (not= x y))

; board constants
(def num-rows 8)
(def row-width 4)
(def top-row 1)
(def bottom-row 8)

(defn compute-pos-neighbors
  "Given a position on a 1-indexed checkerboard, return a list of immediate
   diagonal neighbors"
  [pos]
  (let [current-row         (Math/ceil (/ pos row-width))
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

; ===Start Tests ===========================================

(deftest same-positions
  (is (= false (diff-position? 27 27))))

; not the best test really since there could be collisions but good enough for this
(deftest different-positions
  (is (= true (diff-position? (rand-int 10000000) (rand-int 10000000)))))

(deftest get-neighbors
  (is (= [5 6] (compute-pos-neighbors 2))))


