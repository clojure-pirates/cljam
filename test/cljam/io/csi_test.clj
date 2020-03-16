(ns cljam.io.csi-test
  (:require [clojure.java.io :as cio]
            [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.csi :as csi])
  (:import
   [cljam.io.csi CSI]))

(deftest about-read-index-with-error
  (is (thrown? java.io.IOException (csi/read-index small-bam-file))))

(deftest about-read-index-returns-csi-object
  (is (instance? CSI (csi/read-index test-csi-file))))

(deftest about-read-index-check-the-returning-object
  (let [csi-data ^CSI (csi/read-index test-csi-file)]
    (is (= 4 (.n-ref csi-data)))
    (is (= 14 (.min-shift csi-data)))
    (is (= 6 (.depth csi-data)))
    (is (= 3904 (get-in (.loffset csi-data) [0 1])))
    (is (= 3904 (:beg (get-in (.bidx csi-data) [0 37449 0]))))))

(deftest source-type-test
  (with-open [server (http-server)]
    (are [x] (instance? CSI (csi/read-index x))
      test-csi-file
      (cio/file test-csi-file)
      (cio/as-url (cio/file test-csi-file))
      (cio/as-url (str (:uri server) "/csi/test.csi")))))

(deftest calc-loffsets-test
  (is (= (#'csi/calc-loffsets '(3) '({:beg 1 :end 2 :file-beg 1}
                                     {:beg 5 :end 6 :file-beg 2}) 0)
         {3 1}))

  (is (= (#'csi/calc-loffsets '(3) '({:beg 2 :end 3 :file-beg 1}
                                     {:beg 5 :end 6 :file-beg 2}) 1)
         {3 1}))

  (is (= (#'csi/calc-loffsets '(4) '({:beg 2 :end 3 :file-beg 1}
                                     {:beg 5 :end 6 :file-beg 2}) 1)
         {4 1})))

(deftest-remote large-read-write-test
  (with-before-after {:before (do (prepare-cavia!)
                                  (prepare-cache!)),
                      :after (clean-cache!)}
    (let [temp-csi-file (cio/file temp-dir "test_temp.csi")
          r ^CSI (csi/read-index test-large-vcf-csi-file)
          _ (csi/write-index temp-csi-file r)
          w ^CSI (csi/read-index temp-csi-file)]
      (is (= (.n-ref r) (.n-ref w)))
      (is (= (.min-shift r) (.min-shift w)))
      (is (= (.depth r) (.depth w)))
      (is (= (.bidx r) (.bidx w)))
      (is (= (.loffset r) (.loffset w))))))
