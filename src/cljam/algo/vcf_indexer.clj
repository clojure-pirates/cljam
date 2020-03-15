(ns cljam.algo.vcf-indexer
  "Indexer of VCF."
  (:require [cljam.io.csi :as csi]
            [cljam.io.vcf :as vcf]))

(defn create-index
  "Creates a CSI index file from the VCF file."
  [in-bgziped-vcf out-csi {:keys [shift depth] :or {shift 14 depth 5}}]
  (with-open [r (vcf/reader in-bgziped-vcf)]
    (let [offsets (vcf/read-file-offsets r)
          csi (csi/offsets->index offsets shift depth)]
      (csi/write-index out-csi csi))))
