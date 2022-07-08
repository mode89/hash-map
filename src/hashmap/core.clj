(ns hashmap.core)

(defrecord Map [root])

(defrecord ArrayNode [children children-count])

(defrecord CollisionNode [key-hash children])

(defrecord MapEntry [key-hash key value])

(def EMPTY-MAP (Map. nil))

(defn not-implemented []
  (throw (Exception. "Not implemented")))

(defn array-index [s h]
  "Index of a key inside an ArrayNode"
  (bit-and (unsigned-bit-shift-right h s) 0x1F))

(defn make-entry [k v]
  (MapEntry. (hash k) k v))

(defn make-array-node [node shift]
  (-> (repeat 32 nil)
      vec
      (assoc (array-index shift (:key-hash node)) node)
      (ArrayNode. 1)))

(defn node-get-entry [node shift khash k]
  (cond
    (instance? ArrayNode node)
      (let [child-idx (array-index shift khash)
            child (nth (:children node) child-idx)]
        (if (nil? child)
          nil
          (node-get-entry child (+ shift 5) khash k)))
    (instance? MapEntry node)
      (if (= (:key node) k)
        node
        nil)
    (instance? CollisionNode node)
      (->> (:children node)
           (filter #(= k (:key %)))
           first)
    :else (throw (RuntimeException. "Unexpected type of node"))))

(defn node-assoc [node shift entry]
  (cond
    (instance? ArrayNode node)
      (let [child-idx (array-index shift (:key-hash entry))
            children (:children node)
            child (nth children child-idx)]
        (if (nil? child)
          (-> children
              (assoc child-idx entry)
              (ArrayNode. (inc (:children-count node))))
          (let [new-child (node-assoc child (+ shift 5) entry)]
            (if (identical? child new-child)
              node
              (-> children
                  (assoc child-idx new-child)
                  (ArrayNode. (:children-count node)))))))
    (instance? MapEntry node)
      (if (= (:key node) (:key entry))
        (if (= (:value node) (:value entry))
          node
          entry)
        (if (= (:key-hash node) (:key-hash entry))
          (CollisionNode. (:key-hash entry) [node entry])
          (-> (make-array-node node shift)
              (node-assoc shift entry))))
    (instance? CollisionNode node)
      (if (= (:key-hash node) (:key-hash entry))
        (let [child-idx (-> #(if (= (:key %2) (:key entry)) %1)
                            (keep-indexed (:children node))
                            first)]
          (if (some? child-idx)
            (let [child (nth (:children node) child-idx)]
              (if (= (:value child) (:value entry))
                node
                (CollisionNode. (:key-hash node)
                                (assoc (:children node) child-idx entry))))
            (CollisionNode. (:key-hash node)
                            (conj (:children node) entry))))
        (-> (make-array-node node shift)
            (node-assoc shift entry)))
    :else (throw (RuntimeException. "Unexpected type of node"))))

(defn node-dissoc [node shift khash k]
  (cond
    (instance? ArrayNode node)
      (let [child-idx (array-index shift khash)
            child (nth (:children node) child-idx)]
        (if (nil? child)
          node
          (let [new-child (node-dissoc child (+ shift 5) khash k)]
            (if (identical? child new-child)
              node
              (if (nil? new-child)
                (if (= 1 (:children-count node))
                  nil
                  (ArrayNode. (assoc (:children node) child-idx nil)
                              (dec (:children-count node))))
                (ArrayNode. (assoc (:children node) child-idx new-child)
                            (:children-count node)))))))
    (instance? MapEntry node)
      (if (= (:key node) k)
        nil
        node)
    (instance? CollisionNode node)
      (if (not= (:key-hash node) khash)
        node
        (let [child (->> (:children node)
                         (filter #(= (:key %) k))
                         first)]
          (if (some? child)
            (if (= 2 (count (:children node)))
              ;; If only one child left, convert to MapEntry
              (->> (:children node)
                   (filter #(not= (:key %) k))
                   first)
              ;; Otherwise return new CollisionNode without removed child
              (->> (:children node)
                   (filterv #(not= (:key %) k))
                   (CollisionNode. khash)))
            node)))
    :else (throw (RuntimeException. "Unexpected type of node"))))

(defn entry-difference [ea eb]
  (if (or (identical? ea eb)
          (and (some? ea)
               (some? eb)
               (= (:key ea) (:key eb))
               (= (:value ea) (:value eb))))
    nil
    ea))

(defn node-difference [shift na nb]
  (cond
    (identical? na nb) nil
    (and (some? na) (some? nb))
      (condp instance? na
        MapEntry
          (condp instance? nb
            ArrayNode
              (entry-difference
                na (node-get-entry nb shift (:key-hash na) (:key na)))
            MapEntry
              (entry-difference na nb)
            CollisionNode
              (entry-difference
                na (node-get-entry nb shift (:key-hash na) (:key na)))))
    :else na))

(defn new-map []
  "Create a empty Map"
  EMPTY-MAP)

(defn mget [m k]
  "Get value associated with key `k` inside map `m`"
  (if (nil? (:root m))
    nil
    (-> (:root m)
        (node-get-entry 0 (hash k) k)
        :value)))

(defn massoc [m k v]
  "Associate key `k` with value `v` inside map `m`"
  (let [entry (make-entry k v)]
    (if (nil? (:root m))
      (Map. entry)
      (let [new-root (node-assoc (:root m) 0 entry)]
        (if (identical? new-root (:root m))
          m
          (Map. new-root))))))

(defn mdissoc [m k]
  (if (nil? (:root m))
    m
    (let [new-root (node-dissoc (:root m) 0 (hash k) k)]
      (if (identical? new-root (:root m))
        m
        (if (nil? new-root)
          EMPTY-MAP
          (Map. new-root))))))

(defn mseq [m]
  (if (nil? (:root m))
    nil
    (filter #(instance? MapEntry %)
            (tree-seq
              #(not (instance? MapEntry %))
              #(cond
                (instance? ArrayNode %)
                  (filter some? (:children %))
                (instance? CollisionNode %)
                  (seq (:children %))
                :else (not-implemented))
              (:root m)))))

(defn mkeys [m]
  (if (nil? (:root m))
    nil
    (map #(:key %) (mseq m))))
