(ns cljam.io.fasta.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.test-common :refer :all]
            [cljam.io.fasta.core :as fa-core]))

(def illegal-fasta-file test-tabix-file)

(def temp-test-fa-file (str temp-dir "/test.fa"))
(def temp-medium-fa-file (str temp-dir "/medium.fa"))

(deftest read-headers-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read-headers rdr) test-fa-header))))

(deftest read-indices-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read-indices rdr)
           [{:name "ref" :len 45 :offset 5 :line-blen 45 :line-len 46}
            {:name "ref2" :len 40 :offset 57 :line-blen 40 :line-len 41}]))))

(deftest read-sequences-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read-sequences rdr) test-fa-sequences))))

(deftest read-test
  (with-open [rdr (fa-core/reader test-fa-file)]
    (is (= (fa-core/read rdr) test-fa-data))
    (is (= (fa-core/read rdr) [{:len 0}]))
    (is (not-throw? (fa-core/reset rdr)))
    (is (= (fa-core/read rdr) test-fa-data)))
  (is (thrown? Exception
               (with-open [rdr (fa-core/reader test-tabix-file)]
                 (fa-core/read rdr)))))

(deftest sequential-read-test
  (is (= (fa-core/sequential-read test-fa-file)
         (map #(update % :sequence cstr/upper-case) test-fa-sequences)))
  (is (= (fa-core/sequential-read test-fa-file {:mask? false})
         (map #(update % :sequence cstr/upper-case) test-fa-sequences)))
  (is (= (fa-core/sequential-read test-fa-file {:mask? true})
         test-fa-sequences))
  (is (= (fa-core/sequential-read test-fa-bz2-file)
         (map #(update % :sequence cstr/upper-case) test-fa-sequences)))
  (is (= (fa-core/sequential-read test-fa-bz2-file {:mask? false})
         (map #(update % :sequence cstr/upper-case) test-fa-sequences)))
  (is (= (fa-core/sequential-read test-fa-bz2-file {:mask? true})
         test-fa-sequences))
  (let [fa (fa-core/sequential-read medium-fa-file)]
    (is (= (map :name fa) '("chr1")))
    (is (= (map (comp count :sequence) fa) '(100000))))
  (let [fa (fa-core/sequential-read medium-fa-gz-file)]
    (is (= (map :name fa) '("chr1")))
    (is (= (map (comp count :sequence) fa) '(100000))))
  (is (thrown? Exception (fa-core/sequential-read illegal-fasta-file))))
