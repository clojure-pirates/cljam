(ns cljam.algo.vcf-indexer-test
  "Tests for cljam.algo.bam-indexer."
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.test-common :refer :all]
            [cljam.io.csi :as csi]
            [cljam.io.vcf :as vcf]
            [cljam.algo.vcf-indexer :as vcf-indexer])
  (:import
   [cljam.io.csi CSI]))

(def tmp-csi-file (str temp-dir "/tmp.csi"))
(def tmp-vcf-file (str temp-dir "/tmp.vcf.gz"))
(def tmp-vcf-csi-file (str temp-dir "/tmp.vcf.gz.csi"))

(defn- chunks->maps [bidx]
  (apply merge-with into
         (for [[chr bin-chunks] bidx
               [bin chunks] bin-chunks]
           {chr {bin (mapv #(into {} %) chunks)}})))

(deftest about-vcf-indexer
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw?
         (vcf-indexer/create-index test-vcf-complex-gz-file
                                   tmp-csi-file {:shift 14 :depth 6})))

    (do (vcf-indexer/create-index test-vcf-complex-gz-file
                                  tmp-csi-file {:shift 14 :depth 6})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (.n-ref csi) 4))
          (is (= (.min-shift csi) 14))
          (is (= (.depth csi) 6))
          (is (= (.loffset csi)
                 {0 {1 3904},
                  1 {32769 3973},
                  2 {114704385 4031},
                  3 {1 4783}}))
          (is (= (chunks->maps (.bidx csi))
                 {0 {37449 [{:beg 3904, :end 3973}]},
                  1 {37451 [{:beg 3973, :end 4031}]},
                  2 {44450 [{:beg 4031, :end 4783}]},
                  3 {37449 [{:beg 4783, :end 106168320}]}}))))

    (do (vcf-indexer/create-index test-vcf-complex-gz-file
                                  tmp-csi-file {})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (.n-ref csi) 4))
          (is (= (.min-shift csi) 14))
          (is (= (.depth csi) 5))
          (is (= (.loffset csi)
                 {0 {1 3904},
                  1 {32769 3973},
                  2 {114704385 4031},
                  3 {1 4783}}))
          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 3904, :end 3973}]},
                  1 {4683 [{:beg 3973, :end 4031}]},
                  2 {11682 [{:beg 4031, :end 4783}]},
                  3 {4681 [{:beg 4783, :end 106168320}]}}))))

    (do (vcf-indexer/create-index test-vcf-complex-gz-file
                                  tmp-csi-file {:shift 18 :depth 5})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (.n-ref csi) 4))
          (is (= (.min-shift csi) 18))
          (is (= (.depth csi) 5))
          (is (= (.loffset csi)
                 {0 {1 3904},
                  1 {1 3973},
                  2 {114556929 4031},
                  3 {1 4783}}))
          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 3904, :end 3973}]},
                  1 {4681 [{:beg 3973, :end 4031}]},
                  2 {5118 [{:beg 4031, :end 4783}]},
                  3 {4681 [{:beg 4783, :end 106168320}]}}))))))

(deftest about-vcf-changed-chr-order
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (do (vcf-indexer/create-index test-vcf-changed-chr-order-file
                                  tmp-csi-file {:shift 18 :depth 5})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 2096, :end 2205}]},
                  1 {4681 [{:beg 1647, :end 1811}],
                     4685 [{:beg 1811, :end 2096}]},
                  2 {4681 [{:beg 2205, :end 70975488}]}}))

          (is (= (.loffset csi)
                 {0 {1 2096},
                  1 {1 1647, 1048577 1811},
                  2 {1 2205}}))))))

(deftest about-vcf-changed-chr-order-field-less
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (do (vcf-indexer/create-index test-vcf-changed-chr-order-field-less-file
                                  tmp-csi-file {:shift 18 :depth 5})
        (let [csi ^CSI (csi/read-index tmp-csi-file)]
          (is (= (chunks->maps (.bidx csi))
                 {0 {4681 [{:beg 1597, :end 1761}],
                     4685 [{:beg 1761, :end 2046}]},
                  1 {4681 [{:beg 2046, :end 2155}]},
                  2 {4681 [{:beg 2155, :end 70057984}]}}))

          (is (= (.loffset csi)
                 {0 {1 1597, 1048577 1761},
                  1 {1 2046},
                  2 {1 2155}}))))))

(deftest about-vcf-indexer-input-result
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (cio/copy (cio/file test-vcf-various-bins-gz-file)
              (cio/file tmp-vcf-file))
    (vcf-indexer/create-index tmp-vcf-file tmp-vcf-csi-file
                              {:shift 14 :depth 6})
    (are [chr start end]
         (let [vcf1* (vcf/vcf-reader tmp-vcf-file)
               vcf2* (vcf/vcf-reader test-vcf-various-bins-gz-file)]
           (=  (vcf/read-variants-randomly vcf1*
                                           {:chr chr :start start :end end} {})
               (vcf/read-variants-randomly vcf2*
                                           {:chr chr :start start
                                            :end end} {})))
      "chr1" 1 16384
      "chr1" 1 49153
      "chr1" 1 30000
      "chr1" 49153 147457
      "chr1" 32769 147457
      "chr1" 49153 1064952
      "chr1" 1048577 414826496
      "chr1" 1048577 414826497
      "chr1" 32769 414859265
      "chr1" 414859265 536608769)))
