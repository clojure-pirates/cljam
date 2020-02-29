(ns cljam.io.csi
  "Reader of a CSI format file."
  (:require [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.util.chunk :as chunk]
            [cljam.io.util.bin :as util-bin])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException]))

(deftype CSI [n-ref min-shift depth bidx loffset]
  util-bin/IBinningIndex
  (get-chunks [_ ref-idx bins]
    (vec (mapcat (get bidx ref-idx) bins)))
  (get-min-offset [_ ref-idx beg]
    (if-let [min-offsets (rsubseq (get loffset ref-idx) <= beg)]
      (second (first min-offsets))
      0))
  (get-depth [_]
    depth)
  (get-min-shift [_]
    min-shift))

(def ^:const ^:private csi-magic "CSI\1")

(defn- read-chunks!
  [rdr]
  (let [n-chunk (lsb/read-int rdr)]
    (->> #(let [beg (lsb/read-long rdr) end (lsb/read-long rdr)]
            (chunk/->Chunk beg end))
         (repeatedly n-chunk)
         vec)))

(defn- read-bin-index
  [rdr]
  (let [n-ref (lsb/read-int rdr)]
    (->> #(let [bin (lsb/read-int rdr)
                loffset (lsb/read-long rdr)
                chunks (read-chunks! rdr)]
            (hash-map
             :bin bin
             :loffset loffset
             :chunks chunks))
         (repeatedly n-ref)
         vec)))

(defn- read-index*
  [^DataInputStream rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes csi-magic))
    (throw (IOException. "Invalid CSI file")))
  (let [min-shift (lsb/read-int rdr)
        depth (lsb/read-int rdr)
        l-aux (lsb/read-int rdr)
        _ (lsb/read-bytes rdr l-aux)
        n-ref (lsb/read-int rdr)
        refs (range n-ref)
        bins (vec (repeatedly n-ref #(read-bin-index rdr)))
        max-bin (util-bin/max-bin depth)
        bidx (zipmap refs
                     (map (fn [bin]
                            (into {} (comp (map (juxt :bin :chunks))
                                           (filter #(<= (first %) max-bin)))
                                  bin))
                          bins))
        loffset (zipmap refs
                        (map (fn [bin]
                               (into (sorted-map)
                                     (comp (map (juxt :bin :loffset))
                                           (filter #(<= (first %) max-bin))
                                           (map (fn [[bin loffset]]
                                                  [(util-bin/bin-beg bin
                                                                     min-shift
                                                                     depth)
                                                   loffset])))
                                     bin))
                             bins))]
    (->CSI n-ref min-shift depth bidx loffset)))

(defn read-index
  [f]
  (with-open [r (DataInputStream. (bgzf/bgzf-input-stream f))]
    (read-index* r)))

(defn- calc-bidx [file-offsets shift depth]
  (->> file-offsets
       (map #(assoc %
                    :bin (util-bin/reg->bin (:beg %)
                                            (:end %) shift depth)))
       (reduce (fn [res offset]
                 (if (and (= (:bin (first res)) (:bin offset))
                          (= (:file-end (first res)) (:file-beg offset)))
                   (cons (assoc (first res) :file-end (:file-end offset))
                         (next res))
                   (cons offset res)))
               nil)
       (group-by :bin)
       (map (fn [[bin offsets]]
              [bin (->> offsets
                        (map #(chunk/->Chunk (:file-beg %) (:file-end %)))
                        reverse)]))
       sort
       (into (array-map))))

(defn- calc-loffsets [begs file-offsets]
  (->> begs
       (map (fn [beg]
              [beg (->> (drop-while #(< (:end %) beg) file-offsets)
                        (map :file-beg)
                        first)]))
       (into (array-map))))

(defn offsets->index
  "Calculates loffsets and bidx
   from offsets {:file-beg :file-end :beg :end :chr }"
  [offsets shift depth]
  (let [chr-offsets (->> (partition-by :chr offsets)
                         (map #(vector (:chr (first %)) %))
                         sort
                         (into (array-map)))
        bidx (->> chr-offsets
                  (map (fn [[chr offsets]]
                         [chr (calc-bidx offsets shift depth)]))
                  (into (array-map)))
        loffsets (->> chr-offsets
                      (map (fn [[chr offsets]]
                             [chr (calc-loffsets
                                   (map #(util-bin/bin-beg % shift depth)
                                        (keys (get bidx chr)))
                                   offsets)]))
                      (into (array-map)))]
    [bidx loffsets]))

(defn write-index
  [f ^CSI csi]
  (with-open [w (DataOutputStream. (bgzf/bgzf-output-stream f))]
    (lsb/write-bytes w (.getBytes ^String csi-magic))
    (lsb/write-int w (.min-shift csi))
    (lsb/write-int w (.depth csi))
    (lsb/write-int w 0)
    (lsb/write-int w (count (.bidx csi)))
    (doseq [[chr offsets] (.bidx csi)]
      (lsb/write-int w (count offsets))
      (doseq [[bin chunks] offsets]
        (lsb/write-int w bin)
        (lsb/write-long w (get-in (.loffset csi)
                                  [chr
                                   (util-bin/bin-beg bin
                                                     (.min-shift csi)
                                                     (.depth csi))]))
        (lsb/write-int w (count chunks))
        (doseq [chunk chunks]
          (lsb/write-long w (:beg chunk))
          (lsb/write-long w (:end chunk)))))))
