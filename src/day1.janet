(def sample ["1abc2"
             "pqr3stu8vwx"
             "a1b2c3d4e5f"
             "treb7uchet"])

(def DIGIT-PATTERN
  (peg/compile ~{:main (sequence (some (choice (capture :d) :a)))}))

(peg/match DIGIT-PATTERN "abc1def2ghi")

(defn decode
  [pattern entries]
  (->> entries
     (map (fn [code]
            (let [digits (peg/match pattern code)]
              [(first digits) (when digits (last digits))])))
     (filter (complement (fn [[f l]] (or (nil? f) (nil? l)))))
     (map (fn [[f l]] (string f l)))
     (map parse)
     (reduce + 0)))

(decode DIGIT-PATTERN sample)

(decode DIGIT-PATTERN (string/split "\n" (slurp "src/day1.txt")))

(def sample-2 ["two1nine"
               "eightwothree"
               "abcone2threexyz"
               "xtwone3four"
               "4nineeightseven2"
               "zoneight234"
               "7pqrstsixteen"])

(def DIGIT-MAPPING {"one" "1"
                    "two" "2"
                    "three" "3"
                    "five" "5"
                    "four" "4"
                    "six" "6"
                    "seven" "7"
                    "eight" "8"
                    "nine" "9"})


(defn replace-lettered-digits
  [input]
  (reduce (fn [acc [pattern digit]] (string/replace-all pattern (string pattern digit pattern) acc))
          input (pairs DIGIT-MAPPING)))

(replace-lettered-digits "4nineeightseven2")

(->> sample-2
     (map replace-lettered-digits)
     (decode DIGIT-PATTERN))

(->> (string/split "\n" (slurp "src/day1.txt"))
     (map replace-lettered-digits)
     (decode DIGIT-PATTERN))
