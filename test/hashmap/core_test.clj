(ns hashmap.core_test
  (:require [clojure.test :refer :all]
            [hashmap.core :as hm])
  (:import [hashmap.core Map]))

(deftest new-map
  (let [m (hm/new-map)]
    (is (instance? Map (hm/new-map)))
    (is (nil? (hm/mget m 1)))))

(deftest new-map-massoc
  (let [m1 (hm/new-map)
        m2 (hm/massoc m1 1 2)]
    (is (= 2 (hm/mget m2 1)))))
