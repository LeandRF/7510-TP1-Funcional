(ns unit-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def teams-database "
	racing(leandro).
	river(juan).
	river(agustin).
	racing(roberto).
	boca(alan).
")

(def teams-incomplete-database "
	racing(leandro).
	boca(alan
")

(def fact "racing(leandro, juan).")

;Check news operations
(deftest teams-database-fact-test
  (testing "racing(leandro) should be true"
    (is (= (evaluate-query teams-database "racing(leandro)")
           true))) 
  (testing "river(juan) should be true"
    (is (= (evaluate-query teams-database "river(juan)")
           true))) 
  (testing "boca(juan) should be false"
    (is (= (evaluate-query teams-database "boca(juan)")
           false))) )

;Check query
(deftest check-query-test
  (testing "varon(juan) should be true"
    (is (= (is-ok-query "varon(juan)")
           true)))
  (testing "varonjuan)"" should be false"
    (is (= (is-ok-query "varonjuan)")
           false))))

;Check database
(deftest check-database-test
  (testing "teams-database should be true"
    (is (= (is-ok-database teams-database)
           true)))
  (testing "teams-incomplete-database should be false"
    (is (= (is-ok-database teams-incomplete-database)
           false))))

;Check parser fact
(deftest check-parsefacts-test
  (testing "parser-facts first element of racing(leandro, juan). should be leandro"
    (is (= (first (second (parser-fact  fact )))
           "leandro")))
(testing "parser-facts second element of racing(leandro, juan). should be juan"
   (is (= (second (second (parser-fact  fact )))
         "juan")))         
(testing "parser-facts first element of racing(leandro, juan). shouldnt be leandro"
   (is (not= (second (second (parser-fact  fact)))
         "leandro"))) 
(testing "parser-facts key  of racing(leandro, juan). should be racing"
   (is (= (first (parser-fact  fact))
         "racing")))            
         )

