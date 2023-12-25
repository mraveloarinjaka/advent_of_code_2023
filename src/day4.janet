(import set :as jset)
(import ./utils)

(def SAMPLE
  `
  Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
  Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
  Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
  Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
  Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
  Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
  `)

(def PATTERN 
  ~{:card (sequence "Card" :s* (replace (capture :d+) ,parse))
    :number :d+
    :drawings (some (sequence :s* (replace (capture :number) ,parse) :s*))
    :winning-numbers :drawings
    :main (sequence :card ":" (group :winning-numbers) "|" (group :drawings))})

(peg/match PATTERN "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83")

#part 1
(->> (string/split "\n" (slurp "src/day4.txt"))
     #(string/split "\n" SAMPLE)
     (map (fn [line] (peg/match PATTERN line)))
     (filter (complement nil?))
     (map (fn [[card winnings drawings]]
            (jset/intersect (utils/make-set winnings)
                            (utils/make-set drawings))))
     (map (comp dec length))
     (filter (complement neg?))
     (map (partial math/pow 2))
     (reduce + 0))

#part 2
(defn matching-numbers
  [winnings drawings]
  (length (jset/intersect (utils/make-set winnings)
                          (utils/make-set drawings))))

(defn ->deck
  [input]
  (->> input
       (map (fn [line] (peg/match PATTERN line)))
       (filter (complement nil?))
       (mapcat (fn [[card winnings drawings]]
                 [card #:winnings winnings :drawings drawings
                  @{:matching-numbers (matching-numbers winnings drawings) :copies 1}]))
       (apply table)))

(defn update-deck
  [deck index]
  (let [card (inc index)
        {:matching-numbers matching-numbers :copies copies} (get deck card)]
    (loop [card-to-update :in (range (inc card) (+ (inc card) matching-numbers))]
      (update deck card-to-update (fn [card-data]
                                    (update card-data :copies (partial + copies))))))
  deck)

(let [initial-deck (->deck (string/split "\n" (slurp "src/day4.txt")))
      #initial-deck (->deck (string/split "\n" SAMPLE))
      final-deck (reduce update-deck
                         initial-deck (range (length initial-deck)))]
  (->> (seq [[_ card-data] :pairs final-deck] (get card-data :copies))
       (reduce + 0)))
