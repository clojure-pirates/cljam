(ns cljam.algo.pileup-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.protocols :as p]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util.quality :as qual]
            [cljam.io.sequence :as cseq]
            [cljam.algo.pileup :as plp]
            [clojure.string :as cstr]
            [cljam.io.pileup :as plpio]))

(def test-bam-pileup-ref [0 0 0 0 0 0 1 1 3 3 3 3 3 3 2 3 3 3 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 2 2 2 1 1 1 1 1])
(def test-bam-pileup-ref2 [1 2 2 2 2 3 3 3 3 4 4 5 5 6 6 6 6 6 6 6 5 5 4 4 4 4 4 3 3 3 3 3 3 3 2 1 0 0 0 0])
(def test-bam-mpileup-seq-ref
  '(() () () () () () ("T") ("T") ("A" "A" "A") ("G" "G" "G") ("A" "A" "C") ("T" "T" "T") ("A" "A" "A")
       ("A+4AGAG" "A+2GG" "A") ("G" "G") ("A" "A" "A") ("T" "T" "T") ("A-1N" "A+2AA" "A") ("*" "G") ("C" "C")
       ("T" "T") ("G" ">") (">") (">") (">") (">") (">") (">") (">" "T") (">" "A") (">" "G") (">" "G") (">" "C")
       (">") (">+1C") ;; adjacent indel >+1T
       ("T") ("C" "C") ("A" "A") ("G" "G") ("C" "C") ("G") ("C") ("C") ("A") ("T")))
(def test-bam-mpileup-seq-ref-freq
  [{} {} {} {} {} {} {\T 1} {\T 1} {\A 3} {\G 3} {\A 2 \C 1} {\T 3} {\A 3}
   {\A 3} {\G 2} {\A 3} {\T 3} {\A 3} {\G 1 \* 1} {\C 2}
   {\T 2} {\G 1 \> 1} {\> 1} {\> 1} {\> 1} {\> 1} {\> 1} {\> 1} {\T 1 \> 1} {\A 1 \> 1} {\G 1 \> 1} {\G 1 \> 1} {\C 1 \> 1}
   {\> 1} {\> 1}
   {\T 1} {\C 2} {\A 2} {\G 2} {\C 2} {\G 1} {\C 1} {\C 1} {\A 1} {\T 1}])
(def test-bam-mpileup-seq-ref-ins
  [{} {} {} {} {} {} {} {} {} {} {} {} {}
   {"AGAG" 1 "GG" 1} {} {} {} {"AA" 1} {} {}
   {} {} {} {} {} {} {} {} {} {} {} {} {}
   {} {"C" 1}
   {} {} {} {} {} {} {} {} {} {}])
(def test-bam-mpileup-seq-ref-del
  [{} {} {} {} {} {} {} {} {} {} {} {} {}
   {} {} {} {} {1 1} {} {}
   {} {} {} {} {} {} {} {} {} {} {} {} {}
   {} {}
   {} {} {} {} {} {} {} {} {} {}])
(def test-bam-mpileup-seq-ref2
  '(("A") ("G" "G") ("G" "G") ("T" "T") ("T" "T") ("T" "T" "T") ("T" "T" "T") ("A" "A" "A") ("T" "T" "T")
          ("A" "A" "A" "C") ("A" "A" "A" "A") ("A" "A" "A" "A" "A") ("A" "A" "A" "A" "A") ("C" "C" "C+4AAAT" "T" "T" "T")
          ("A" "A" "A" "A" "A" "A") ("A" "A" "A" "A" "A" "A") ("A" "A" "T" "T" "T" "T") ("T" "T" "T" "T" "T" "T")
          ("A" "A" "A" "A" "A" "A") ("A" "A" "A" "A" "A" "A") ("T" "G" "G" "G" "G") ("T" "T" "T" "T" "T")
          ("C" "C" "C" "C") ("T" "T" "T" "T") ("A" "A" "A" "A") ("C" "C" "C" "C") ("A" "A" "A" "A") ("G" "G" "G")
          ("A" "A" "A") ("G" "G" "G") ("C" "C" "C") ("A" "A" "A") ("A" "A" "A") ("C" "C" "C") ("T" "T") ("A") () () () ()))
(def test-bam-mpileup-seq-ref2-freq
  [{\A 1} {\G 2} {\G 2} {\T 2} {\T 2} {\T 3} {\T 3} {\A 3} {\T 3}
   {\A 3 \C 1} {\A 4} {\A 5} {\A 5} {\C 3 \T 3}
   {\A 6} {\A 6} {\A 2 \T 4} {\T 6}
   {\A 6} {\A 6} {\T 1 \G 4} {\T 5}
   {\C 4} {\T 4} {\A 4} {\C 4} {\A 4} {\G 3}
   {\A 3} {\G 3} {\C 3} {\A 3} {\A 3} {\C 3} {\T 2} {\A 1} {} {} {} {}])
(def test-bam-mpileup-seq-ref-with-ref
  '(() () () () () () (".") (".") ("." "." ".") ("." "." ".") ("." "." "C") ("." "." ".") ("." "." ".")
       (".+4AGAG" ".+2GG" ".") ("." ".") ("." "." ".") ("." "." ".") (".-1G" ".+2AA" ".") ("*" ".") ("." ".")
       ("." ".") ("." ">") (">") (">") (">") (">") (">") (">") (">" ".") (">" ".") (">" ".") (">" ".") (">" ".")
       (">") (">+1C") ;; adjacent indel >+1T
       (".") ("." ".") ("." ".") ("." ".") ("." ".") (".") (".") (".") (".") (".")))
(def test-bam-mpileup-qual-ref
  '([] [] [] [] [] [] [\~] [\~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~]
       [\~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~] [\~] [\~] [\~]
       [\~] [\~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~] [\~] [\~] [\~ \~] [\~ \~] [\~ \~] [\~ \~]
       [\~] [\~] [\~] [\~] [\~]))

(defn- ->aln [m]
  (-> {:qname "DUMMY", :flag 0, :rname "DUMMY", :pos -1, :end -1, :rnext "DUMMY", :pnext -1, :tlen -1, :mapq -1, :cigar "10M", :seq "AAAAAAAAAA", :qual "IIIIIIIIII"}
      (merge m)
      (update :pos int)
      (update :end int)
      (update :flag int)
      (update :tlen int)
      (update :pnext int)
      (update :mapq int)
      p/map->SAMAlignment))

(defn- ->pbase [m]
  (plpio/map->PileupBase (merge {:start? false :reverse? false :end? false} m)))

(defn- ->pile [[pos pile]]
  [pos (map (juxt :pos :end) pile)])

(deftest about-pileup-seq

  ;; ----------
  ;; 1234567890...
  (is (= (map ->pile (plp/pileup-seq 1 2 (mapv ->aln [{:pos 1 :cigar "10M" :end 10}])))
         [[1 [[1 10]]]
          [2 [[1 10]]]]))

  ;;    ----------
  ;;   ----------
  ;;  ----------
  ;; ----------
  ;; 1234567890123...
  (is (= (map (comp count second)
              (plp/pileup-seq 1 20 (map #(->aln (hash-map :pos (inc %) :cigar "10M" :end (+ % 10))) (range))))
         [1 2 3 4 5 6 7 8 9 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map (comp count second)
              (plp/pileup-seq 101 120 (map #(->aln (hash-map :pos (inc %) :cigar "10M" :end (+ % 10))) (range))))
         [10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]))
  (is (= (map :pos
              (second (last (plp/pileup-seq 1 100000 (map #(->aln (hash-map :pos (inc %) :cigar "10M" :end (+ % 10))) (range))))))
         [99991 99992 99993 99994 99995 99996 99997 99998 99999 100000]))

  ;;     -----
  ;;    ----
  ;;   ---
  ;;  --
  ;; -
  ;; 1234567890...
  (is (= (map (comp count second)
              (plp/pileup-seq 1 10 (map #(->aln (hash-map :pos (inc %) :cigar (str (inc %) "M") :end (+ % (inc %)))) (range))))
         [1 1 2 2 3 3 4 4 5 5]))

  ;;       --------
  ;;      ----------
  ;;     --
  ;;    ----
  ;;   ------
  ;;  --------
  ;; ----------
  ;; 1234567890...
  (is (= (map (comp count second)
              (plp/pileup-seq 1 10 (map #(->aln (hash-map :pos (inc %) :cigar (str (- 10 (* (mod % 5) 2)) "M") :end (+ % (- 10 (* (mod % 5) 2))))) (range))))
         [1 2 3 4 5 6 6 6 6 6])))

(deftest pileup-seq
  (testing "empty"
    (is (nil? (seq (plp/pileup-seq 1 10 []))))
    (is (nil? (seq (plp/pileup-seq 2 10 (map ->aln [{:pos 1 :end 1}])))))
    (is (nil? (seq (plp/pileup-seq 1 10 (map ->aln [{:pos 11 :end 11}]))))))
  (testing "dense"
    (are [?in ?out]
         (= ?out (mapv ->pile (plp/pileup-seq 1 10 (mapv ->aln (map #(zipmap [:pos :end] %) ?in)))))
      [[1 1]] [[1 [[1 1]]]]
      [[3 5]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]]]
      [[3 5] [4 4]] [[3 [[3 5]]] [4 [[3 5] [4 4]]] [5 [[3 5]]]]
      [[3 5] [4 6]] [[3 [[3 5]]] [4 [[3 5] [4 6]]] [5 [[3 5] [4 6]]] [6 [[4 6]]]]))
  (testing "sparse"
    (are [?in ?out]
         (= ?out (mapv ->pile (plp/pileup-seq 1 10 (mapv ->aln (map #(zipmap [:pos :end] %) ?in)))))
      [[3 5] [14 16]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]]]
      [[3 5] [6 7]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]] [6 [[6 7]]] [7 [[6 7]]]]
      [[3 5] [4 5] [6 7]] [[3 [[3 5]]] [4 [[3 5] [4 5]]] [5 [[3 5] [4 5]]] [6 [[6 7]]] [7 [[6 7]]]]
      [[3 5] [7 8] [10 11]] [[3 [[3 5]]] [4 [[3 5]]] [5 [[3 5]]] [7 [[7 8]]] [8 [[7 8]]] [10 [[10 11]]]])))

(deftest align-pileup-seqs
  (testing "empty"
    (is (empty? (plp/align-pileup-seqs)))
    (is (empty? (plp/align-pileup-seqs [])))
    (is (empty? (plp/align-pileup-seqs [] []))))
  (testing "single"
    (are [?in ?out]
         (= ?out (plp/align-pileup-seqs ?in))
      [{:pos 3 :pile [{:pos 3 :end 5}]}]
      [[3 [{:pos 3 :pile [{:pos 3 :end 5}]}]]]

      [{:pos 3 :pile [[{:pos 3 :end 5}]]}
       {:pos 4 :pile [[{:pos 3 :end 5}]]}
       {:pos 5 :pile [[{:pos 3 :end 5}]]}]
      [[3 [{:pos 3 :pile [[{:pos 3 :end 5}]]}]]
       [4 [{:pos 4 :pile [[{:pos 3 :end 5}]]}]]
       [5 [{:pos 5 :pile [[{:pos 3 :end 5}]]}]]]

      [{:pos 3 :pile [{:pos 3 :end 5}]}
       {:pos 4 :pile [{:pos 3 :end 5} {:pos 4 :end 4}]}
       {:pos 5 :pile [{:pos 3 :end 5}]}
       {:pos 8 :pile [{:pos 8 :end 8}]}]
      [[3 [{:pos 3 :pile [{:pos 3 :end 5}]}]]
       [4 [{:pos 4 :pile [{:pos 3 :end 5} {:pos 4, :end 4}]}]]
       [5 [{:pos 5 :pile [{:pos 3 :end 5}]}]]
       [8 [{:pos 8 :pile [{:pos 8 :end 8}]}]]]))
  (testing "multiple"
    (is (= [[3 [{:pos 3, :pile [{:pos 3, :end 5}]} nil]]
            [4 [{:pos 4, :pile [{:pos 3, :end 5}]} {:pos 4, :pile [{:pos 4, :end 6}]}]]
            [5 [{:pos 5, :pile [{:pos 3, :end 5}]} {:pos 5, :pile [{:pos 4, :end 6}]}]]
            [6 [nil {:pos 6, :pile [{:pos 4, :end 6}]}]]]
           (plp/align-pileup-seqs [{:pos 3 :pile [{:pos 3 :end 5}]}
                                   {:pos 4 :pile [{:pos 3 :end 5}]}
                                   {:pos 5 :pile [{:pos 3 :end 5}]}]
                                  [{:pos 4 :pile [{:pos 4 :end 6}]}
                                   {:pos 5 :pile [{:pos 4 :end 6}]}
                                   {:pos 6 :pile [{:pos 4 :end 6}]}])))
    (is (= [[3 [{:pos 3, :pile [{:pos 3, :end 5}]} nil]]
            [4 [{:pos 4, :pile [{:pos 3, :end 5}]} nil]]
            [5 [{:pos 5, :pile [{:pos 3, :end 5}]} nil]]
            [8 [nil {:pos 8, :pile [{:pos 8, :end 9}]}]]
            [9 [nil {:pos 9, :pile [{:pos 8, :end 9}]}]]]
           (plp/align-pileup-seqs [{:pos 3 :pile [{:pos 3 :end 5}]}
                                   {:pos 4 :pile [{:pos 3 :end 5}]}
                                   {:pos 5 :pile [{:pos 3 :end 5}]}]
                                  [{:pos 8 :pile [{:pos 8 :end 9}]}
                                   {:pos 9 :pile [{:pos 8 :end 9}]}])))
    (is (= [[3 [{:pos 3, :pile [{:pos 3, :end 4}]} nil {:pos 3, :pile [{:pos 3, :end 3}]} nil]]
            [4 [{:pos 4, :pile [{:pos 3, :end 4}]} {:pos 4, :pile [{:pos 4, :end 4}]} nil nil]]]
           (plp/align-pileup-seqs [{:pos 3 :pile [{:pos 3 :end 4}]}
                                   {:pos 4 :pile [{:pos 3 :end 4}]}]
                                  [{:pos 4 :pile [{:pos 4 :end 4}]}]
                                  [{:pos 3 :pile [{:pos 3 :end 3}]}]
                                  [])))))

(deftest chunk-step-seq
  (testing "empty"
    (is (nil? (seq (#'plp/seq-step 1 10 2 []))))
    (is (nil? (seq (#'plp/seq-step 2 10 2 (map ->aln [{:pos 1 :end 1}])))))
    (is (nil? (seq (#'plp/seq-step 1 10 2 (map ->aln [{:pos 11 :end 11}]))))))
  (testing "dense"
    (are [?in ?out]
         (= ?out (mapv ->pile (#'plp/seq-step 1 10 2 (mapv ->aln (map #(zipmap [:pos :end] %) ?in)))))
      [[1 1]] [[1 [[1 1]]]]
      [[3 5]] [[3 [[3 5]]]]
      [[3 5] [4 4]] [[3 [[3 5] [4 4]]]]
      [[3 5] [4 6]] [[3 [[3 5] [4 6]]] [6 [[4 6]]]]))
  (testing "sparse"
    (are [?in ?out]
         (= ?out (mapv ->pile (#'plp/seq-step 1 10 2 (mapv ->aln (map #(zipmap [:pos :end] %) ?in)))))
      [[3 5] [14 16]] [[3 [[3 5]]]]
      [[3 5] [6 7]] [[3 [[3 5]]] [6 [[6 7]]]]
      [[3 5] [4 5] [6 7]] [[3 [[3 5] [4 5]]] [6 [[6 7]]]]
      [[3 5] [7 8] [10 11]] [[3 [[3 5]]] [7 [[7 8]]] [10 [[10 11]]]]))
  (testing "wide"
    (are [?in ?out]
         (= ?out (mapv ->pile (#'plp/seq-step 1 10 20 (mapv ->aln (map #(zipmap [:pos :end] %) ?in)))))
      [[1 1] [14 16]] [[1 [[1 1]]]]
      [[3 5] [4 6] [20 21]] [[3 [[3 5] [4 6]]]])))

(deftest about-pileup
  (testing "dense"
    (with-open [br (sam/bam-reader test-sorted-bam-file)
                fr (cseq/fasta-reader test-fa-file)]
      (let [plp-ref (doall (plp/pileup br {:chr "ref"}))
            plp-ref2 (doall (plp/pileup br {:chr "ref2"}))]
        (is (= (filter pos? test-bam-pileup-ref)
               (map (comp count :pile) plp-ref)))
        (is (= (keep-indexed #(when (pos? %2) (inc %1)) test-bam-pileup-ref)
               (map :pos plp-ref)))
        (is (= (remove empty? test-bam-mpileup-seq-ref-freq)
               (map #(frequencies (map :base (:pile %))) plp-ref)))
        (is (= (keep #(seq (map qual/fastq-char->phred-byte %)) test-bam-mpileup-qual-ref)
               (map #(map :qual (:pile %)) plp-ref)))
        (is (= (filter pos? test-bam-pileup-ref2)
               (map (comp count :pile) plp-ref2)))
        (is (= (remove empty? test-bam-mpileup-seq-ref2-freq)
               (map #(frequencies (map :base (:pile %))) plp-ref2)))))))

(deftest mpileup
  (with-open [r1 (sam/reader test-sorted-bam-file)
              r2 (sam/reader r1)]
    (let [mplp (plp/mpileup {:chr "ref"} {} r1 r2)]
      (is (= (keep-indexed #(when (pos? %2) (inc %1)) test-bam-pileup-ref)
             (map first mplp))))))

(deftest pileup-region
  (with-open [br (sam/bam-reader test-sorted-bam-file)]
    (let [plp-ref1 (doall (plp/pileup br {:chr "ref" :start 1 :end 40}))
          plp-ref2 (doall (plp/pileup br {:chr "ref2" :start 1 :end 40}))]
      (is (= (filter pos? (take 40 test-bam-pileup-ref))
             (map (comp count :pile) plp-ref1)))
      (is (= (filter pos? (take 40 test-bam-pileup-ref2))
             (map (comp count :pile) plp-ref2)))
      (is (= (->> test-bam-pileup-ref
                  (take 40)
                  (map vector (range))
                  (filter (comp pos? second))
                  (map (comp inc first)))
             (map :pos plp-ref1)))
      (is (= (->> test-bam-pileup-ref2
                  (take 40)
                  (map vector (range))
                  (filter (comp pos? second))
                  (map (comp inc first)))
             (map :pos plp-ref2)))
      (is (= (filter seq (take 40 test-bam-mpileup-seq-ref-freq))
             (map #(frequencies (map :base (:pile %))) plp-ref1)))
      (is (= (filter seq (take 40 test-bam-mpileup-seq-ref2-freq))
             (map #(frequencies (map :base (:pile %))) plp-ref2))))))

;;   --------
;;   --------
;; 1234567890
(def ^:private reads-for-pileup
  (mapv
   p/map->SAMAlignment
   [{:qname "R001" :flag (int 99) :rname "seq1" :pos (int 3) :end (int 10) :seq "AATTGGCCAA" :qual "AABBCCDDz~" :cigar "2S8M"
     :rnext "=" :pnext (int 3) :tlen (int 8) :mapq (int 60)}
    {:qname "R001" :flag (int 147) :rname "seq1" :pos (int 3) :end (int 10) :seq "TTGGCCAATT" :qual "AABBCCDDz~" :cigar "8M2S"
     :rnext "=" :pnext (int 3) :tlen (int -8) :mapq (int 20)}]))

;;       ----
;;   ---
;; 1234567890
(def ^:private reads-gap-for-pileup
  (mapv
   p/map->SAMAlignment
   [{:qname "R001" :flag (int 99) :rname "seq1" :pos (int 3) :end (int 5) :seq "AATTG" :qual "AABz~" :cigar "2S3M"
     :rnext "=" :pnext (int 7) :tlen (int 5) :mapq (int 60)}
    {:qname "R001" :flag (int 147) :rname "seq1" :pos (int 7) :end (int 10) :seq "TTGGTT" :qual "CCDDz~" :cigar "4M2S"
     :rnext "=" :pnext (int 3) :tlen (int -5) :mapq (int 20)}]))

;;       ----
;;   ----
;; 1234567890
(def ^:private reads-continal-for-pileup
  (mapv
   p/map->SAMAlignment
   [{:qname "R001" :flag (int 99) :rname "seq1" :pos (int 3) :end (int 6) :seq "AATTGG" :qual "AABBz~" :cigar "2S4M"
     :rnext "=" :pnext (int 6) :tlen (int 5) :mapq (int 60)}
    {:qname "R001" :flag (int 147) :rname "seq1" :pos (int 7) :end (int 10) :seq "TTGGTT" :qual "CCDDz~" :cigar "4M2S"
     :rnext "=" :pnext (int 3) :tlen (int -5) :mapq (int 20)}]))

;;        --------
;;   --------
;; 123456789012345
(def ^:private reads-overlapping-for-pileup
  (mapv
   p/map->SAMAlignment
   [{:qname "R001" :flag (int 147) :rname "seq1" :pos (int 3) :end (int 10) :seq "AATTGGCCAA" :qual "AABBCCDDz~" :cigar "2S8M"
     :rnext "=" :pnext (int 7) :tlen (int 6) :mapq (int 60)}
    {:qname "R001" :flag (int 99) :rname "seq1" :pos (int 8) :end (int 15) :seq "CCCTTGGATT" :qual "z~AABBCCDD" :cigar "8M2S"
     :rnext "=" :pnext (int 3) :tlen (int -6) :mapq (int 20)}]))

;;     ----
;;   --------
;; 1234567890
(def ^:private reads-including-for-pileup
  (mapv
   p/map->SAMAlignment
   [{:qname "R001" :flag (int 99) :rname "seq1" :pos (int 3) :end (int 10) :seq "AATTGGCCAA" :qual "AABBCCDDz~" :cigar "2S8M"
     :rnext "=" :pnext (int 5) :tlen (int 8) :mapq (int 60)}
    {:qname "R001" :flag (int 147) :rname "seq1" :pos (int 5) :end (int 8) :seq "GGACTT" :qual "AABBz~" :cigar "4M2S"
     :rnext "=" :pnext (int 3) :tlen (int -8) :mapq (int 20)}]))

(defn- pileup* [region options xs]
  (plp/pileup
   (reify p/IAlignmentReader
     (p/read-refs [_]
       [{:name "seq1", :len 249250621}])
     (p/read-alignments [_ _]
       xs))
   region
   options))

(deftest pileup-default
  (let [plps (->> reads-for-pileup
                  (pileup* {:chr "seq1" :start 1 :end 10} {}))
        plps-gap (->> reads-gap-for-pileup
                      (pileup* {:chr "seq1" :start 1 :end 10} {}))
        plps-cont (->> reads-continal-for-pileup
                       (pileup* {:chr "seq1" :start 1 :end 10} {}))
        plps-overlap (->> reads-overlapping-for-pileup
                          (pileup* {:chr "seq1" :start 1 :end 10} {}))
        plps-include (->> reads-including-for-pileup
                          (pileup* {:chr "seq1" :start 1 :end 10} {}))
        plps-chunk (->> reads-overlapping-for-pileup
                        (pileup* {:chr "seq1" :start 1 :end 15} {:chunk-size 2}))]
    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\C] [\C] [\A] [\A]])
           (map #(map :base (:pile %)) plps)))
    (is (= (filter seq [[] [] [65] [65] [67] [67] [69] [69] [124] [128]])
           (map #(map :qual (:pile %)) plps)))

    (is (= (filter pos? [0 0 1 1 1 0 1 1 1 1])
           (map (comp count :pile) plps-gap)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [] [\T] [\T] [\G] [\G]])
           (map #(map :base (:pile %)) plps-gap)))
    (is (= (filter seq [[] [] [33] [89] [93] [] [34] [34] [35] [35]])
           (map #(map :qual (:pile %)) plps-gap)))

    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps-cont)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\T] [\T] [\G] [\G]])
           (map #(map :base (:pile %)) plps-cont)))
    (is (= (filter seq [[] [] [33] [33] [89] [93] [34] [34] [35] [35]])
           (map #(map :qual (:pile %)) plps-cont)))

    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps-overlap)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\C] [\C] [\C] [\A]])
           (map #(map :base (:pile %)) plps-overlap)))
    (is (= (filter seq [[] [] [33] [33] [34] [34] [35] [124] [74] [74]])
           (map #(map :qual (:pile %)) plps-overlap)))

    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps-include)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\C] [\C] [\A] [\A]])
           (map #(map :base (:pile %)) plps-include)))
    (is (= (filter seq [[] [] [33] [33] [66] [66] [28] [68] [89] [93]])
           (map #(map :qual (:pile %)) plps-include)))

    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps-chunk)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\C] [\C] [\C] [\A] [\T] [\T] [\G] [\G] [\A]])
           (map #(map :base (:pile %)) plps-chunk)))
    (is (= (filter seq [[] [] [33] [33] [34] [34] [35] [124] [74] [74] [32] [33] [33] [34] [34]])
           (map #(map :qual (:pile %)) plps-chunk)))))

(deftest overlap-correction
  (let [plps (->> reads-for-pileup
                  (pileup* {:chr "seq1" :start 1 :end 10} {:ignore-overlaps? true}))]
    (is (= (filter pos? [0 0 2 2 2 2 2 2 2 2])
           (map (comp count :pile) plps)))
    (is (= (filter seq [[] [] [\T \T] [\T \T] [\G \G] [\G \G] [\C \C] [\C \C] [\A \A] [\A \A]])
           (map #(map :base (:pile %)) plps)))
    (is (= (filter seq [[] [] [33 32] [33 32] [34 33] [34 33] [35 34] [35 34] [89 35] [93 35]])
           (map #(map :qual (:pile %)) plps)))))

(deftest filter-by-base-quality
  (let [plps (->> reads-for-pileup
                  (pileup* {:chr "seq1" :start 1 :end 10} {:min-base-quality 0}))]
    (is (= (filter pos? [0 0 2 2 2 2 2 2 2 2])
           (map (comp count :pile) plps)))
    (is (= (filter seq [[] [] [\T \T] [\T \T] [\G \G] [\G \G] [\C \C] [\C \C] [\A \A] [\A \A]])
           (map #(map :base (:pile %)) plps)))
    (is (= (filter seq [[] [] [65 0] [65 0] [67 0] [67 0] [69 0] [69 0] [124 0] [128 0]])
           (map #(map :qual (:pile %)) plps)))))

(deftest filter-by-map-quality
  (let [plps (->> reads-for-pileup
                  (pileup* {:chr "seq1" :start 1 :end 10} {:min-map-quality 30}))]
    (is (= (filter pos? [0 0 1 1 1 1 1 1 1 1])
           (map (comp count :pile) plps)))
    (is (= (filter seq [[] [] [\T] [\T] [\G] [\G] [\C] [\C] [\A] [\A]])
           (map #(map :base (:pile %)) plps)))
    (is (= (filter seq [[] [] [33] [33] [34] [34] [35] [35] [89] [93]])
           (map #(map :qual (:pile %)) plps)))))

(deftest about-create-mpileup
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [out-file (str temp-dir "/test.pileup")]
      (is (not-throw? (plp/create-mpileup test-sorted-bam-file out-file)))
      (doseq [[r1 r2] (->> [test-pileup-file out-file]
                           (map (comp cstr/split-lines slurp))
                           (apply map vector))
              :when (not (cstr/starts-with? r1 "ref\t35\t"))] ;; adjacent indel
        (is (= r1 r2))))))
