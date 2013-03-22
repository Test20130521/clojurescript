(ns cljs.repl.reflect
  (:refer-clojure :exclude [macroexpand])
  (:require [cljs.analyzer :as analyzer]
            [cljs.compiler :as compiler]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn- get-meta [sym]
  (let [ns (symbol (namespace sym))
        n  (symbol (name sym))]
    (if-let [sym-meta (get (:defs (get @analyzer/namespaces ns)) n)]
      (-> (select-keys sym-meta #{:name :method-params :doc :line :file})
          (update-in [:name] str)
          ; TODO not clear on why we need to stringify the method params;
          ; not doing so yields an exception browser-side when evaluating the
          ; compiled javascript. Stuck with read-string'ing :method-params when
          ; we print docs in cljs.repl.browser.reflect for now.
          (update-in [:method-params] #(str (vec %)))))))

(defn macroexpand [form]
  "Fully expands a cljs macro form."
  (let [mform (analyzer/macroexpand-1 {} form)]
    (if (identical? form mform)
      mform
      (recur mform))))

(defn- read-url-string
  [s]
  (read-string (java.net.URLDecoder/decode s "UTF-8")))

(defn- compile-and-return
  "Compiles a form to javascript and returns it on conn."
  [req form]
  (let [ast (analyzer/analyze {:ns {:name 'cljs.user}} form)
        js  (try (compiler/emit-str ast)
                 (catch Exception e (println e)))]
    (#'cljs.repl.browser/send-response req 200 js :content-type "text/javascript")))

(defn- request-params
  [req]
  (apply hash-map (-> req .getRequestURI .getRawQuery (str/split #"="))))

; cljs.repl.browser implicitly loads this ns; yeah, I know, I'm a bad person,
; but these tools do zero good if they're not loaded, and it's too late if
; you've already turned your REPL into a browser-repl (unless you have multiple
; REPL sessions....)
(defmethod @#'cljs.repl.browser/handle-get "/reflect/macroexpand"
  [{:keys [http-exchange session-id]}]
  (let [mform (-> http-exchange request-params (get "form") read-url-string macroexpand)]
    (#'cljs.repl.browser/send-response http-exchange 200 (with-out-str (pprint/pprint mform)))))

(defmethod @#'cljs.repl.browser/handle-get "/reflect/var-meta"
  [{:keys [http-exchange session-id]}]
  (let [meta (-> http-exchange request-params (get "var") read-url-string get-meta)]
    (compile-and-return http-exchange meta)))

