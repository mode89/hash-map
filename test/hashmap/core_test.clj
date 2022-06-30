(ns hashmap.core_test
  (:require [clojure.test :refer :all]
            [hashmap.core :refer :all])
  (:import [hashmap.core Map]))

(deftype Key [value hash-code]
  Object
  (equals [this other]
    (= value (.value other)))
  (hashCode [this]
    hash-code))

(defn mkey [x]
  (Key. x x))

(deftest make-new-map
  (let [m (new-map)]
    (is (instance? Map (new-map)))
    (is (nil? (mget m 1)))))

(deftest new-map-massoc
  (let [m1 (new-map)
        m2 (massoc m1 1 2)]
    (is (= 2 (mget m2 1)))))

(deftest massoc-same-key-value
  (let [m1 (new-map)
        m2 (massoc m1 (mkey 1) 2)
        m3 (massoc m2 (mkey 1) 2)]
    (is (identical? m2 m3))))

(deftest massoc-same-key
  (let [m1 (new-map)
        m2 (massoc m1 (mkey 1) 2)
        m3 (massoc m2 (mkey 1) 3)]
    (is (= 3 (mget m3 (mkey 1))))))

(deftest massoc-another
  (let [m1 (new-map)
        m2 (massoc m1 (mkey 1) 2)
        m3 (massoc m2 (mkey 2) 3)]
    (is (not (identical? m2 m3)))
    (is (= 2 (mget m3 (mkey 1))))
    (is (= 3 (mget m3 (mkey 2))))))

(deftest massoc-same-array-index
  (let [m1 (new-map)
        m2 (massoc m1 (mkey 1) 2)
        m3 (massoc m2 (mkey 33) 3)]
    (is (= 2 (mget m3 (mkey 1))))
    (is (= 3 (mget m3 (mkey 33))))))

(deftest collision
  (let [m (-> (new-map)
              (massoc (Key. 1 42) 2)
              (massoc (Key. 3 42) 4))]
    (is (= 2 (mget m (Key. 1 42))))
    (is (= 4 (mget m (Key. 3 42))))))

(deftest overwrite-collision-node
  (let [m (-> (new-map)
              (massoc (Key. 1 42) 2)
              (massoc (Key. 3 42) 4)
              (massoc (Key. 1 42) 5))]
    (is (= 5 (mget m (Key. 1 42))))
    (is (= 4 (mget m (Key. 3 42))))))

(deftest keep-collision-node
  (let [m1 (-> (new-map)
               (massoc (Key. 1 42) 2)
               (massoc (Key. 3 42) 4))
        m2 (massoc m1 (Key. 1 42) 2)]
    (is (identical? m1 m2))))

(deftest add-to-collision-node
  (let [m (-> (new-map)
              (massoc (Key. 1 42) 2)
              (massoc (Key. 3 42) 4)
              (massoc (Key. 5 42) 6))]
    (is (= 2 (mget m (Key. 1 42))))
    (is (= 4 (mget m (Key. 3 42))))
    (is (= 6 (mget m (Key. 5 42))))))

(deftest expand-collision-node
  (let [m (-> (new-map)
              (massoc (Key. 1 42) 2)
              (massoc (Key. 3 42) 4)
              (massoc (Key. 5 5) 6))]
    (is (= 2 (mget m (Key. 1 42))))
    (is (= 4 (mget m (Key. 3 42))))
    (is (= 6 (mget m (Key. 5 5))))))

(deftest dissoc-empty
  (let [m1 (new-map)
        m2 (mdissoc m1 42)]
    (is (identical? m1 m2))))

(deftest dissoc-map-entry-miss
  (let [m1 (new-map)
        m2 (massoc m1 1 42)
        m3 (mdissoc m2 2)]
    (is (identical? m2 m3))))

(deftest dissoc-map-entry-hit
  (let [m1 (new-map)
        m2 (massoc m1 1 42)
        m3 (mdissoc m2 1)]
    (is (identical? m1 m3))))

(deftest dissoc-collision-node-miss-hash
  (let [m1 (-> (new-map)
               (massoc (Key. 1 42) 2)
               (massoc (Key. 3 42) 4))
        m2 (mdissoc m1 (Key. 2 43))]
    (is (identical? m1 m2))))

(deftest dissoc-collision-node-miss-key
  (let [m1 (-> (new-map)
               (massoc (Key. 1 42) 2)
               (massoc (Key. 3 42) 4))
        m2 (mdissoc m1 (Key. 2 42))]
    (is (identical? m1 m2))))

(deftest dissoc-collision-node-hit
  (let [m1 (-> (new-map)
               (massoc (Key. 1 42) 2)
               (massoc (Key. 3 42) 4))
        m2 (mdissoc m1 (Key. 3 42))]
    (is (= 2 (mget m2 (Key. 1 42))))
    (is (nil? (mget m2 (Key. 3 42))))))

(deftest dissoc-collision-node-hit-3
  (let [m1 (-> (new-map)
               (massoc (Key. 1 42) 2)
               (massoc (Key. 3 42) 4)
               (massoc (Key. 5 42) 6))
        m2 (mdissoc m1 (Key. 3 42))]
    (is (= 2 (mget m2 (Key. 1 42))))
    (is (nil? (mget m2 (Key. 3 42))))
    (is (= 6 (mget m2 (Key. 5 42))))))
