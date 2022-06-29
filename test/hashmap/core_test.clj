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
