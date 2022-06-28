(ns hashmap.core)

(defrecord Map [root])

(defrecord ArrayNode [children])

(defrecord MapEntry [key value])

(def EMPTY-ARRAY-NODE (-> (repeat 32 nil)
                          vec
                          ArrayNode.))

(defn array-index [s h]
  "Index of a key inside an ArrayNode"
  (bit-and (unsigned-bit-shift-right h s) 0x1F))

(defmulti node-get-entry (fn [node shift khash k] (class node)))

(defmethod node-get-entry MapEntry [node shift khash k]
  (if (= (:key node) k)
    node
    nil))

(defmethod node-get-entry ArrayNode [node shift khash k]
  (let [child-idx (array-index shift khash)
        child (nth (:children node) child-idx)]
    (if (nil? child)
      nil
      (node-get-entry child (+ shift 5) khash k))))

(defmulti node-assoc (fn [node shift khash k v] (class node)))

(defmethod node-assoc MapEntry [node shift khash k v]
  (if (= (:key node) k)
    (if (= (:value node) v)
      node
      (MapEntry. k v))
    (if (<= shift 30)
      (-> EMPTY-ARRAY-NODE
          (node-assoc shift (.hashCode (:key node)) (:key node) (:value node))
          (node-assoc shift khash k v))
      (throw (Exception. "Not implemented")))))

(defmethod node-assoc ArrayNode [node shift khash k v]
  (let [child-idx (array-index shift khash)
        child (nth (:children node) child-idx)]
    (if (nil? child)
      (-> (:children node)
          (assoc child-idx (MapEntry. k v))
          ArrayNode.)
      (throw (Exception. "Not implemented")))))

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
