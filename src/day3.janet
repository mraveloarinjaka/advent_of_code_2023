(import spork/misc)
(import ./coordinates)

(def SAMPLE
  `
  467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..
  `
  )

(defn parse-part-number
  [start part-number end]
  [:part-number (parse part-number) (range start end)])

(defn parse-symbol
  [start sym end]
  [:symbol sym [start]])

(def PARTS-EXTRACTOR
  (peg/compile 
    ~{:white-space (set ".")
      :part-number (sequence (position) (capture :d+) (position))
      :symbol (sequence (position) (capture (if-not (choice :white-space :part-number) 1)) (position))
      :main (sequence (any (choice (replace :part-number ,parse-part-number)
                                   (replace :symbol ,parse-symbol)
                                   :white-space)))}))

(peg/match PARTS-EXTRACTOR "617*......")

(defn ->coordinates-2d
  [line-nb line]
  (seq [[item-type item-value positions] :in line]
    [item-type item-value (map (fn [col-nb] [col-nb line-nb]) positions)]))

(defn ->game-data
  [input]
  (let [split-data (->> (string/split "\n" input)
                        (filter (complement empty?)))
        height (length split-data)
        width (length (first split-data))
        game-data (map (partial peg/match PARTS-EXTRACTOR) split-data)
        game-data-indexed (zipcoll (range height) game-data)]
    {:height height :width width
     :data (mapcat (fn [[line-nb line]]
                  (->coordinates-2d line-nb line))
                (pairs game-data-indexed))}))

(defn symbol?
  [[item-type _ _]]
  (= :symbol item-type))

(defn make-set
  [coll]
  (zipcoll coll coll))

(defn ->symbols-neighbors
  [{:height height
    :width width
    :data data}]
  (->> data
       (filter symbol?)
       (mapcat (fn [[_ _ positions]] positions))
       (mapcat (partial coordinates/->valid-neigbors width height))
       make-set
       ))

(->symbols-neighbors (->game-data SAMPLE))

(defn part?
  [[item-type _ _]]
  (= :part-number item-type))

(defn neighbor?
  [neighbors position]
  (get neighbors position false))

(let [game-data (->game-data SAMPLE)
      {:height height
       :width width
       :data data} game-data
      symbols-neighbors (->symbols-neighbors game-data)]
  (->> data
       (filter part?)
       (filter (fn [[_ _ positions]]
                 (some (partial neighbor? symbols-neighbors) positions)))
       (map (fn [[_ item-value _]] item-value))
       (reduce + 0)))

(let [game-data (->game-data (slurp "src/day3.txt"))
      {:height height
       :width width
       :data data} game-data
      symbols-neighbors (->symbols-neighbors game-data)]
  (->> data
       (filter part?)
       (filter (fn [[_ _ positions]]
                 (some (partial neighbor? symbols-neighbors) positions)))
       (map (fn [[_ item-value _]] item-value))
       (reduce + 0)))
