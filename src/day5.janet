(import spork/ev-utils :as jutils)

(def SAMPLE
  `
  seeds: 79 14 55 13

  seed-to-soil map:
  50 98 2
  52 50 48

  soil-to-fertilizer map:
  0 15 37
  37 52 2
  39 0 15

  fertilizer-to-water map:
  49 53 8
  0 11 42
  42 0 7
  57 7 4

  water-to-light map:
  88 18 7
  18 25 70

  light-to-temperature map:
  45 77 23
  81 45 19
  68 64 13

  temperature-to-humidity map:
  0 69 1
  1 0 69

  humidity-to-location map:
  60 56 37
  56 93 4
  `)

(def PATTERN
  ~{:number (sequence (replace (capture :d+) ,parse) :s*)
    :seeds (sequence "seeds: " (constant ,:seeds) (some :number))
    :map (sequence (constant ,:map) (repeat 3 :number))
    :seed-to-soil (sequence (constant ,:seed-to-soil) "seed-to-soil")
    :soil-to-fertilizer (sequence (constant ,:soil-to-fertilizer) "soil-to-fertilizer")
    :fertilizer-to-water (sequence (constant ,:fertilizer-to-water) "fertilizer-to-water")
    :water-to-light (sequence (constant ,:water-to-light) "water-to-light")
    :light-to-temperature (sequence (constant ,:light-to-temperature) "light-to-temperature")
    :temperature-to-humidity (sequence (constant ,:temperature-to-humidity) "temperature-to-humidity")
    :humidity-to-location (sequence (constant ,:humidity-to-location) "humidity-to-location")
    :title (sequence (constant :title) (choice :seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light :light-to-temperature :temperature-to-humidity :humidity-to-location))
    :main (choice :seeds :title :map)})

(defn ->almanac
  [input]
  (->> input
       (map (partial peg/match PATTERN))
       (filter (complement nil?))
       (reduce (fn [almanac line]
                 (let [{:current-map current-map} almanac]
                   (match line
                     [:seeds & seeds] (put almanac :seeds seeds)
                     [:title title] (put almanac :current-map title)
                     [:map destination source nb] 
                     (let [to-insert [destination source nb]
                           existing (get almanac current-map)]
                       (if existing
                         (array/push existing to-insert)
                         (put almanac current-map @[to-insert])))))
                 almanac)
               @{:current-map :seeds})))

(def MAPPINGS [:seed-to-soil 
               :soil-to-fertilizer 
               :fertilizer-to-water 
               :water-to-light 
               :light-to-temperature 
               :temperature-to-humidity 
               :humidity-to-location])

(defn seed->location 
  [almanac seed]
  (let [location (reduce
                   (fn [index mapping]
                     (let [mappings (get almanac mapping)
                           [result _] (reduce (fn [[so-far found?] [destination source nb]]
                                                (if (not found?)
                                                  (if (and (<= source so-far) (< so-far (+ nb source)))
                                                    (let [steps (- so-far source)
                                                          next-index (+ destination steps)]
                                                      [next-index true])
                                                    [so-far false])
                                                  [so-far true]))
                                              [index false] mappings)]
                       #(pp (string mapping ":" index "->" result))
                       result))
                   seed MAPPINGS)]
    #(pp (string seed " -> " location))
    location))

#part 1
(let [almanac (->>
                #(string/split "\n" SAMPLE)
                (string/split "\n" (slurp "src/day5.txt"))
                ->almanac)]
  (->> (get almanac :seeds)
       #(map (partial seed->location almanac))
       #(apply min)
       ))

#part 2
(def none @[])

(defn intersection-and-relative-complements
  "Find the intersection between segments [x y] and [a b] and list the segments in [x y] not in [a b]"
  [[x y] [a b]]
  (let [result (cond 
                 (or (< b x) (< y a)) [none [[x y]]]
                 (and (< x a) (< b y)) [[a b] [[x (- a 1)] [(+ b 1) y]]]
                 (and (<= a x) (<= x b)) [[x (min y b)] (if (< b y) [[(+ b 1) y]] [])]
                 (and (<= a y) (<= y b)) [[(max x a) y] (if (< x a) [[x (- a 1)]] [])]
                 )]
    #(pp (string/format "%m ^ %m -> %m" [x y] [a b] result))
    result))

#(loop [x :in (range 1 11)
#       y :in (range 1 11)
#       :when (< x y)]
#  (intersection-and-outers [x y] [3 7]))

(defn ->range [a n] [a (+ a (- n 1))])

(defn find-destination-range
  [[x y] [destination source steps]]
  (let [source-range (->range source steps) 
        [xiyi relative-complements] (intersection-and-relative-complements [x y] source-range)]
    (if (= xiyi none)
      [none relative-complements]
      (let [[xi yi] xiyi
            dx (- xi source)
            dy (- yi source)]
        [[(+ destination dx) (+ destination dy)] relative-complements]))))

(defn find-destination-ranges
  [mappings [x y]]
  (let [mappings-sorted-by-source (sort-by (fn [[_ s _]] s) mappings)
        {:destination-ranges destination-ranges :unmapped-ranges unmapped-ranges}
        (reduce (fn [res mapping]
                  (let 
                    [{:destination-ranges destinations-so-far :unmapped-ranges unmappeds-so-far} res
                     new-unmappeds @[]]
                    (loop [unmapped :in unmappeds-so-far]
                      (let [[new-intersection new-unmappeds-so-far] (find-destination-range unmapped mapping)]
                        (when (not= new-intersection none) (array/push destinations-so-far new-intersection))
                        (put res :unmapped-ranges new-unmappeds-so-far))))
                  res)
                @{:destination-ranges @[] :unmapped-ranges @[[x y]]}
                mappings)]
    (array/concat destination-ranges unmapped-ranges)))

(defn find-all-destination-ranges
  [mappings sources]
  (mapcat (partial find-destination-ranges mappings) sources))

#water-to-light map:
#88 18 7
#18 25 70

#(let [almanac (->> 
#                (string/split "\n" SAMPLE)
#                ->almanac)
#      sample-sources [[18 20] [50 60]] 
#      sample-segment (get-in almanac [:water-to-light 0])
#      sample-mappings (get almanac :water-to-light)]
#  (find-all-destination-ranges sample-mappings sample-sources))

(defn seeds->seed-ranges
  [seeds]
  (->> seeds
       (partition 2)
       (map (fn [[a n]] (->range a n)))))

(let [almanac (->> 
                #(string/split "\n" SAMPLE)
                (string/split "\n" (slurp "src/day5.txt"))
                ->almanac)
      _ (update almanac :seeds seeds->seed-ranges)
      seeds (get almanac :seeds)
      all-destinations (reduce (fn [sources mappings-index]
                                 (let [destinations (find-all-destination-ranges (get almanac mappings-index) sources)]
                                   #(pp (string/format "%m - %m  -> %m" mappings-index sources destinations))
                                   destinations))
                               seeds MAPPINGS)]
  (->> all-destinations
       (sort-by first)
       first
       first))

#light-to-temperature map:
#45 77 23
#81 45 19
#68 64 13

#":light-to-temperature - @[(74 87) (46 50) (55 62)]  -> @[(78 81) (46 55) (82 86) (91 98)]"
#(find-destination-ranges
#  @[[45 77 23] [81 45 19] [68 64 13]] [74 87]) 


