(defn ->xy
  [width index]
  [(mod index width)
   (div index width)])

(defn ->index 
  [width [x y]]
  (+ x (* width y)))

(defn ->neighbors
  [[x y]]
  {:north [x (dec y)]
   :south [x (inc y)]
   :west [(dec x) y]
   :east [(inc x) y]
   :north-west [(dec x) (dec y)]
   :north-east [(inc x) (dec y)]
   :south-west [(dec x) (inc y)]
   :south-east [(inc x) (inc y)]
   })

(defn- valid?
  [width height [x y]]
  (and (<= 0 x) (< x width) (<= 0 y) (< y height)))

(defn ->valid-neigbors
  [width height [x y]]
  (filter (fn [[xi yi]]
            (valid? width height [xi yi]))
          (->neighbors [x y])))

(comment

  (->xy 10 12)
  (->neighbors [0 0])

  (->index 10 [2 1])

  (comment))
