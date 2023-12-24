(import spork/misc)
(import ./coordinates)
(import ./utils)

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

(defn ->neighbors
  [item-filter
   {:height height
    :width width
    :data data}]
  (->> data
       (filter item-filter)
       (mapcat (fn [[_ _ positions]] positions))
       (mapcat (partial coordinates/->valid-neigbors width height))
       utils/make-set))

(defn ->symbols-neighbors
  [game-data]
  (->neighbors symbol? game-data))

#_(->symbols-neighbors (->game-data SAMPLE))

(defn part?
  [[item-type _ _]]
  (= :part-number item-type))

(defn neighbor?
  [neighbors position]
  (get neighbors position false))

# part 1
(let [game-data (->game-data (slurp "src/day3.txt"))
      #game-data (->game-data SAMPLE)
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

# part 2
(let [game-data (->game-data (slurp "src/day3.txt"))
      #game-data (->game-data SAMPLE)
      {:height height
       :width width
       :data data} game-data
      parts (->> data
                 (filter (complement symbol?)))
      gears (->> data
                 (filter symbol?)
                 (filter (fn[[_ item-value _]] (= "*" item-value))))]
  (->> gears
       (map (fn [[item-type item-value positions]]
              [item-type item-value positions
               (utils/make-set (mapcat (partial coordinates/->valid-neigbors width height) positions))]))
       (map (fn [[item-type item-value positions neighbors]]
              (let [neighboring-parts (filter
                                        (fn [[_ _ positions]]
                                          (some (partial neighbor? neighbors) positions))
                                        parts)]
                [item-type item-value positions neighboring-parts])))
       (filter (fn [[_ _ _ neighboring-parts]]
                 (= 2 (length neighboring-parts))))
       (map (fn [[_ _ _ neighboring-parts]]
              (->> neighboring-parts
                   (map misc/second)
                   (reduce * 1))))
       (reduce + 0)))

