(ns cljam.algo.vcf-indexer
  "Indexer of BAM."
  (:require [cljam.io.csi :as csi]
            [cljam.io.vcf :as vcf]))

(defn create-index
  "Creates a CSI index file from the VCF file."
  [in-bgziped-vcf out-csi {:keys [shift depth] :or {shift 14 depth 5}}]
  (with-open [r (vcf/reader in-bgziped-vcf)]
     (let [[bidx loffset] (vcf/read-file-index r shift depth)
           csi (csi/->CSI (count (keys bidx)) shift depth bidx loffset)]
        (csi/write-index out-csi csi))))
