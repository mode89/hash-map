(ns hashmap.core)

(defrecord Map [root])

(defrecord ArrayNode [children])

(defrecord MapEntry [key value])

(defn arr-idx [s h]
  "Index of a key inside an ArrayNode"
  (bit-and (unsigned-bit-shift-right h s) 0x1F))

(defmulti node-get-entry (fn [node shift khash k] (class node)))

(defmethod node-get-entry MapEntry [node shift khash k]
  (if (= (:key node) k)
    node
    nil))

(defmulti node-assoc (fn [node shift khash k v] (class node)))

(defmethod node-assoc MapEntry [node shift khash k v]
  (if (= (:key node) k)
    (if (= (:value node) v)
      node
      (MapEntry. k v))
    (throw (Exception. "Not implemented"))))

(defn new-map []
  "Create a empty Map"
  (Map. nil))

(defn mget [m k]
  "Get value associated with key `k` inside map `m`"
  (if (nil? (:root m))
    nil
    (-> (:root m)
        (node-get-entry 0 (.hashCode k) k)
        :value)))

(defn massoc [m k v]
  "Associate key `k` with value `v` inside map `m`"
  (if (nil? (:root m))
    (Map. (MapEntry. k v))
    (let [new-root (node-assoc (:root m) 0 (.hashCode k) k v)]
      (if (identical? new-root (:root m))
        m
        (Map. new-root)))))
