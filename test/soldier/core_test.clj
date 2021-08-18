(ns soldier.core-test
  (:require [clojure.test :refer :all]
            [soldier.core :refer :all]))
(def size 3)

(defn mk-sut
  [& {:keys [name life location] :or {name "rayen" life 100 location {:x 0 :y 0}}}]
  (mk-soldier name :life life :location location))

(deftest testing-soldier-functions []
  (testing "testing go left"
     (let [sut (mk-sut :location {:x 1 :y 1})
           sut (go-left sut size)]
       (is (= {:x 0 :y 1} (:location sut)))
       (is (= {:x 0 :y 1} (:location (go-left sut size))))))
   (testing "when go right"
     (let [sut (mk-sut :location {:x 1 :y 1})
           sut (go-right sut size)]
       (is (= {:x 2 :y 1} (:location sut)))
       (is (= {:x 2 :y 1} (:location (go-right sut size))))))
   (testing "testing go up"
     (let [sut (mk-sut :location {:x 1 :y 1})
           sut (go-up sut size)]
       (is (= {:x 1 :y 0} (:location sut)))
       (is (= {:x 1 :y 0} (:location (go-up sut size))))))
   (testing "when go down"
     (let [sut (mk-sut :location {:x 1 :y 1})
           sut (go-down sut size)]
       (is (= {:x 1 :y 2} (:location sut)))
       (is (= {:x 1 :y 2} (:location (go-down sut size))))))
  (testing "when invoke is-dead? and life is lower than 1 should return true other wise should return false"
     (is (false? (is-dead? (mk-sut))))
     (is (true? (is-dead? (mk-sut :life 0)))))
  (testing "fight"
    (let [s1 (mk-sut :name "winner" :life 1000)
          s2 (mk-sut :name "looser" :life 50)
          score (fight s1 s2 (fn [x y]))]
     (is (= (:name (:win score) "winner")))
     (is (= (:name (:losser score) "looser")))))
  )

(deftest run-game []
  (testing "run game"
    (is (not (nil? (game "s1" "s2" size print-moved print-attacked print-game-started))))))

