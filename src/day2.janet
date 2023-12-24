(def SAMPLE ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
             "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
             "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
             "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
             "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(def CONSTRAINTS {:red 12 :green 13 :blue 14})

(defn parse-game-id
  [game-id]
  #{:game (parse game-id)}
  (parse game-id))

(defn parse-subset-count
  [subset-count]
  #[:subset-count (parse subset-count)]
  (parse subset-count)
  )

(defn parse-subset-color
  [subset-color]
  #[:subset-color (keyword subset-color)]
  (keyword subset-color)
  )

(def GAME_DATA_EXTRACTOR
  ~{:game-id (sequence "Game " (replace (capture :d+) ,parse-game-id))
    :subset (sequence (replace (capture :d+) ,parse-subset-count)
              " "
              (replace (capture (choice "red" "green" "blue")) ,parse-subset-color))
    :reveal (some (choice (group :subset) (opt ", ")))
    :game (some (choice :reveal (opt "; ")))
    :main (sequence :game-id ": " :game)})

(peg/match GAME_DATA_EXTRACTOR "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defn ->game-data
  [game]
  (peg/match GAME_DATA_EXTRACTOR game))

(defn valid-subset?
  [constraints [nb color]]
  (<= nb (get constraints color)))

(defn valid-game?
  [constraints [id & subsets]]
  #(print (string "Game " id))
  (all (partial valid-subset? constraints) subsets))

(valid-game? CONSTRAINTS (->game-data "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

(->> SAMPLE
     (map ->game-data)
     (filter (partial valid-game? CONSTRAINTS))
     (map first)
     (reduce + 0))

(->> (string/split "\n" (slurp "src/day2.txt"))
     (map ->game-data)
     (filter (complement nil?))
     (filter (partial valid-game? CONSTRAINTS))
     (map first)
     (reduce + 0))

(defn ->subset-count
  [[nb _]]
  nb)

(defn ->subset-color
  [[_ color]]
  color)

(defn ->min-set-cubes
  [game-data]
  (let [[_ & subsets] game-data]
    (->> (group-by ->subset-color subsets)
         (map (partial sort-by ->subset-count))
         (map last))))

(->min-set-cubes "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(->> SAMPLE
     (map ->game-data)
     (map ->min-set-cubes)
     (map (partial map first))
     (map (partial reduce * 1)) #power
     (reduce + 0))

(->> (string/split "\n" (slurp "src/day2.txt"))
     (map ->game-data)
     (filter (complement nil?))
     (map ->min-set-cubes)
     (map (partial map first))
     (map (partial reduce * 1)) #power
     (reduce + 0)
     )


