(ns clj-money.util)

(defn println-and-return
  "Prints the specified information and then returns it"
  ([caption to-print] (println-and-return caption identity to-print))
  ([caption transform-fn to-print]
   (println (str "*** " caption " ***"))
   (if (coll? to-print)
     (doseq [p to-print]
       (println (str "  " (transform-fn p))))
     (println (str "  " (transform-fn to-print))))
   (println (str "*** end - " caption " ***"))))
