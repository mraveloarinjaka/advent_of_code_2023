(import set :as jset)

(defn make-set
  [coll]
  (apply jset/frozen coll))
