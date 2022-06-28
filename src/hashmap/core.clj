(ns hashmap.core)

(defrecord Map [root])

(defrecord ArrayNode [children])

(defrecord MapEntry [key value])

(defn arr-idx [s h]
  "Index of a key inside an ArrayNode"
  (bit-and (unsigned-bit-shift-right h s) 0x1F))

(defn new-map []
  "Create a empty Map"
  (Map. nil))

(defn mget [m k]
  "Get value associated with key `k` inside map `m`"
  (if (nil? (:root m))
    nil
    (let [khash (.hashCode k)
          idx (arr-idx 0 khash)]
      (-> m :root :children (aget idx) :value))))

(defn massoc [m k v]
  "Associate key `k` with value `v` inside map `m`"
  (let [khash (.hashCode k)
        idx (arr-idx 0 khash)]
    (if (nil? (:root m))
      (let [arr (object-array 32)]
        (aset arr idx (MapEntry. k v))
        (-> arr ArrayNode. Map.)))))
