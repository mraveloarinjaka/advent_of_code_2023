(def SAMPLE
  `
  Time:      7  15   30
  record:  9  40  200
  `)

(defn ->races
  [input]
  (let [[time record] (->> input
                             (string/split "\n")
                             (map (partial string/split " "))
                             (map (partial drop 1))
                             (map (partial filter (complement empty?)))
                             (map (partial map parse)))]
    (seq [index :in (range (length time))]
            [(get time index) (get record index)])))

(defn count-victories
  [[time record]]
  (->> (seq [hold :in (range (inc time))]
         (let [remaining-time (- time hold)
               speed hold]
           (* speed remaining-time)))
       (filter (partial < record))
       length))

(->> 
  #SAMPLE
  (slurp "src/day6.txt")
  ->races
  (map count-victories)
  (reduce * 1))


