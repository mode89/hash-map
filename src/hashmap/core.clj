(ns hashmap.core)

(defrecord Map [root])

(defrecord ArrayNode [children])

(defrecord CollisionNode [key-hash children])

(defrecord MapEntry [key value])

(defn array-index [s h]
  "Index of a key inside an ArrayNode"
  (bit-and (unsigned-bit-shift-right h s) 0x1F))

(defn make-array-node [shift entry]
  (-> (repeat 32 nil)
      vec
      (assoc (->> (:key entry)
                  .hashCode
                  (array-index shift))
             entry)
      ArrayNode.))

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

(defmethod node-get-entry CollisionNode [node shift khash k]
  (->> (:children node)
       (filter #(= k (:key %)))
       first))

(defmulti node-assoc (fn [node shift khash k v] (class node)))

(defmethod node-assoc MapEntry [node shift khash k v]
  (if (= (:key node) k)
    (if (= (:value node) v)
      node
      (MapEntry. k v))
    (if (= khash (.hashCode (:key node)))
      (CollisionNode. khash [node (MapEntry. k v)])
      (-> (make-array-node shift node)
          (node-assoc shift khash k v)))))

(defmethod node-assoc ArrayNode [node shift khash k v]
  (let [child-idx (array-index shift khash)
        children (:children node)
        child (nth children child-idx)]
    (if (nil? child)
      (-> children
          (assoc child-idx (MapEntry. k v))
          ArrayNode.)
      (let [new-child (node-assoc child (+ shift 5) khash k v)]
        (if (identical? child new-child)
          node
          (-> children
              (assoc child-idx new-child)
              ArrayNode.))))))

(defmethod node-assoc CollisionNode [node shift khash k v]
  (if (= (:key-hash node) khash)
    (let [child-idx (-> #(if (= k (:key %2)) %1)
                        (keep-indexed (:children node))
                        first)]
      (if (some? child-idx)
        (let [child (nth (:children node) child-idx)]
          (if (= (:value child) v)
            (throw (Exception. "Not implemented"))
            (CollisionNode. khash
                            (assoc (:children node)
                                   child-idx
                                   (MapEntry. k v)))))
        (throw (Exception. "Not implemented"))))
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
