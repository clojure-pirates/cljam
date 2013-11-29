(ns cljam.pileup.mpileup
  (:require [clojure.string :as str]
            [cljam.cigar :as cgr]
            [cljam.sequence :as cseq]
            [cljam.io :as io]
            [cljam.util :refer [ubyte]]
            [cljam.util.sam-util :refer [phred->fastq fastq->phred]]
            [cljam.pileup.common :refer [window-width step center]]
            [cljam.pileup.pileup :refer [rpositions]]))

(defn- wrap
  [rname positions counts seq qual]
  (map (fn [{:keys [pos count qual seq]}]
         {:rname rname
          :pos pos
          :ref \N ; FIXME
          :count count
          :seq seq
          :qual qual})
       (map merge
            (map (partial hash-map :pos) positions)
            (map (partial hash-map :count) counts)
            (map (partial hash-map :qual) qual)
            (map (partial hash-map :seq) seq))))

(defn- pickup-qual [aln pos]
  (if (= (:qual aln) "*")
    \~
    (nth (:qual aln) pos)))

(defn- encode-seq* [seq*]
  (loop [[f & r] (filter #(nil? (#{\P} (:op %))) seq*)
         ret     []
         op      nil
         tmp     {:n 0, :op nil, :seq nil}]
    (if (nil? f)
      (case op
        \M (apply conj ret (map str (:seq tmp)))
        \I (if (seq ret)
             (update-in ret [(dec (count ret))] str "+" (:n tmp) (apply str (:seq tmp)))
             ret)
        \D (apply conj ret (map str (:seq tmp)))
        \N (apply conj ret (map str (:seq tmp)))
        ret)
      (if (nil? op)
        (recur r ret (:op f) f)
        (if (= (:op f) op)
          (recur r ret (:op f) (-> tmp
                                   (update-in [:n] + (:n f))
                                   (assoc :op (:op f))
                                   (update-in [:seq] (partial apply conj) (:seq f))))
          (let [new-ret (case op
                          \M (apply conj ret (map str (:seq tmp)))
                          \I (if (seq ret)
                               (update-in ret [(dec (count ret))] str "+" (:n tmp) (apply str (:seq tmp)))
                               ret)
                          \D (apply conj ret (map str (:seq tmp)))
                          \N (apply conj ret (map str (:seq tmp)))
                          ret)]
            (recur r new-ret (:op f) f)))))))

(defn- encode-seq
  "Encode sequence strings for mpileup output.
  e.g. ({:n 2, :op \\M :seq [\\T \\A]} ...) => \"^?TA...$\""
  [seq*]
  (let [seq** (encode-seq* seq*)]
    (-> (update-in seq** [(dec (count seq**))] str "$") ; Append "$" to the end
        (update-in [0] #(str "^?" %)))))                ; Insert "^?" before the begin

(defn- count-for-alignment
  [^clojure.lang.PersistentHashMap aln
   ^String rname
   ^clojure.lang.LazySeq positions]
  (if (= rname (:rname aln))
    (let [left  (:pos aln)
          right (dec (+ left (cgr/count-ref (:cigar aln))))
          seq*  (encode-seq (cseq/parse (:seq aln) (:cigar aln)))]
      (map (fn [p]
             (if (and (>= p left) (<= p right))
               {:count 1
                :seq   (nth seq* (- p left))
                :qual  (pickup-qual aln (- p left))
                :head  (= p left)
                :tail  (= p right)}
               {:count 0, :seq nil, :qual nil}))
           positions))
    (take (count positions) (repeat {:count 0, :seq nil, :qual nil}))))

(defn- count-for-positions
  [alns rname positions]
  (if (pos? (count alns))
    (let [cfas (map #(count-for-alignment % rname positions) alns)]
      (wrap rname positions
            (apply map (fn [& args]
                         (reduce + (map :count args))) cfas)
            (apply map (fn [& args]
                         (str/join
                          (map :seq args)))
                   cfas)
            (apply map (fn [& args]
                         (str/join
                          (map :qual args)))
                   cfas)))
    (wrap rname positions
          (take (count positions) (repeat 0))
          nil nil)))

(defn- read-alignments
  [rdr ^String rname ^Long rlength ^Long pos]
  (let [^Long left (let [^Long val (- pos window-width)]
                     (if (< val 0)
                       0
                       val))
        ^Long right (let [^Long val (+ pos window-width)]
                      (if (< rlength val)
                        rlength
                        val))]
    (io/read-alignments rdr {:chr rname
                             :start left
                             :end right
                             :depth :deep})))

(defn- search-ref
  [refs rname]
  (first
   (filter (fn [r] (= (:name r) rname))
           refs)))

(defn- pileup*
  ([rdr ^String rname ^Long rlength ^Long start ^Long end]
     (flatten
      (let [parts (partition-all step (rpositions start end))]
        (map (fn [positions]
               (let [^Long pos (if (= (count positions) step)
                                 (nth positions center)
                                 (nth positions (quot (count positions) 2)))
                     ^clojure.lang.LazySeq alns (read-alignments rdr rname rlength pos)]
                 (count-for-positions alns rname positions)))
             parts)))))

(defn pileup
  ([rdr ^String rname]
     (pileup rdr rname -1 -1))
  ([rdr ^String rname ^Long start* ^Long end*]
     (let [r (search-ref (.refs rdr) rname)]
       (if (nil? r)
         nil
         (pileup* rdr
                  rname (:len r)
                  (if (neg? start*) 0 start*)
                  (if (neg? end*) (:len r) end*))))))