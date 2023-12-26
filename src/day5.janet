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
    :title (choice :seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light :light-to-temperature :temperature-to-humidity :humidity-to-location)
    :main (choice :seeds :title :map)})

(->> (string/split "\n" SAMPLE)
     (map (partial peg/match PATTERN))
     (filter (complement nil?))
     (reduce (fn [almanac line]
               (let [{:current-map current-map} almanac]
                 (match line
                   [:seeds & seeds] (put almanac :seeds seeds)
                   [:seed-to-soil] (put almanac :current-map :seed-to-soil)
                   [:soil-to-fertilizer] (put almanac :current-map :soil-to-fertilizer)
                   [:fertilizer-to-water] (put almanac :current-map :fertilizer-to-water)
                   [:water-to-light] (put almanac :current-map :water-to-light)
                   [:light-to-temperature] (put almanac :current-map :light-to-temperature)
                   [:temperature-to-humidity] (put almanac :current-map :temperature-to-humidity)
                   [:humidity-to-location] (put almanac :current-map :humidity-to-location)
                   [:map destination source nb] (put almanac :map [destination source nb])))
               almanac)
             @{:current-map :seeds}) 
     )

