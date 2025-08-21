#!/usr/bin/env bb

;; csv2md.bb - Convert CSV to Markdown table
;;
;; Usage:
;; - Reads from a file: -f path/to/file.csv or positional FILE
;; - Reads from stdin when no file is provided or when FILE is "-"
;; - Delimiter: -d "," (default). Supports "\t" or "tab" for TSV.

(ns csv2md
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(def usage
  (str
   "csv2md - Convert CSV to Markdown table\n\n"
   "Options:\n"
   "  -f, --file FILE       Input CSV file path (use '-' for stdin)\n"
   "  -d, --delimiter CHAR  Field delimiter (default ','. Use 'tab' or \\t for tabs)\n"
   "  -h, --help            Show this help\n"))

(defn parse-args [args]
  (loop [m {:delimiter \,, :file nil}
         args args]
    (if (empty? args)
      m
      (let [[a & rest] args]
        (cond
          (or (= a "-h") (= a "--help")) (assoc m :help true)
          (or (= a "-f") (= a "--file")) (recur (assoc m :file (first rest)) (rest rest))
          (or (= a "-d") (= a "--delimiter"))
          (let [ds (or (first rest) ",")
                ;; allow literal "\\t", actual tab char, or the word "tab"
                ds (if (= ds "\\t") "\t" ds)
                delim (if (#{"tab" "\t"} ds)
                        \tab
                        (if (seq ds) (first ds) \,))]
            (recur (assoc m :delimiter delim) (rest rest)))
          :else
          ;; treat first non-flag token as file
          (recur (assoc m :file a) rest))))))

(defn read-rows [{:keys [file delimiter]}]
  (let [file (when (and file (not (str/blank? file))) file)
        from-stdin (or (nil? file) (= file "-"))]
    (if from-stdin
      (let [s (slurp *in*)]
        (when (not (str/blank? s))
          (csv/read-csv s :separator delimiter)))
      (with-open [r (io/reader file)]
        (doall (csv/read-csv r :separator delimiter))))))

(defn escape-cell [s]
  (-> (or s "")
      (str/replace "|" "\\|")
      (str/replace #"\r?\n" "<br>")))

(defn row->md [row]
  (str "| " (str/join " | " (map escape-cell row)) " |"))

(defn separator-row [n]
  (str "| " (str/join " | " (repeat n "---")) " |"))

(defn -main [& args]
  (let [{:keys [help] :as opts} (parse-args args)]
    (if help
      (do (println usage) (System/exit 0))
      (let [rows (or (read-rows opts) [])]
        (if (empty? rows)
          (do (binding [*out* *err*]
                (println "No input. Provide a file with -f/--file or pipe data to stdin.")
                (println)
                (println usage))
              (System/exit 1))
          (let [header (first rows)
                body   (rest rows)
                cols   (count header)]
            (println (row->md header))
            (println (separator-row cols))
            (doseq [r body]
              (println (row->md r)))))))))

(apply -main *command-line-args*)
