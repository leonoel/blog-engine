(ns blog-engine
  (:require [ring.adapter.jetty :as j]
            [compojure.core :refer [defroutes GET]]
            [compojure.handler :as ch]
            [missionary.core :as m]
            [clj-http.client :as http]
            [markdown.core :as md]
            [hickory.core :as h]
            [hickory.zip :as hz]
            [hiccup.core :as hiccup]
            [garden.core :as garden]
            [glow.parse :as gp]
            [glow.html :as gh]
            [clojure.edn :as edn]
            [clojure.zip :as zip]
            [clout.core :as clout]
            [ring.util.codec :as codec]
            [medley.core :as medley])
  (:import (java.util.concurrent Future)
           (java.io PushbackReader StringReader)))

(defn str-env
  ([name] (str-env name nil))
  ([name not-found] (or (System/getenv name) not-found)))

(defn read-env
  ([name] (read-env name nil))
  ([name not-found]
   (if-some [s (str-env name nil)]
     (read-string s) not-found)))

(defn http-get [url]
  (fn [success! failure!]
    (let [fut (http/get url {:async? true
                             :oncancel #(failure! (ex-info "GET request interrupted." {:url url}))}
                        success! failure!)]
      #(.cancel ^Future fut true))))

(def base-url (str-env "BASE_URL"))

(def get-style
  (http-get (str base-url "/style.edn")))

(def get-pages
  (http-get (str base-url "/pages.edn")))

(defn get-markdown [id]
  (http-get (str base-url "/" id ".md")))

(defn highlight-clojure [hiccup]
  (loop [loc (hz/hiccup-zip hiccup)]
    (if (zip/end? loc)
      (zip/root loc)
      (let [[tag attr body] (zip/node loc)]
        (-> (if (and (= :code tag) (= "clojure" (:class attr)))
              (zip/replace loc [:code (gh/hiccup-transform (gp/parse body))]) loc)
            (zip/next)
            (recur))))))

(def with-meta-headers
  (partial reduce-kv
           (fn [l k v]
             (zip/append-child l [:meta {:name (name k) :content v}]))))

(defn with-headers [hiccup meta]
  (-> (hz/hiccup-zip hiccup)
      (zip/down)
      (zip/down)
      (zip/append-child [:title (:title meta)])
      (zip/append-child [:link {:rel "stylesheet" :type "text/css" :href "/style"}])
      (zip/append-child [:meta {:charset "UTF-8"}])
      (with-meta-headers (dissoc meta :title))
      (zip/root)))

(defn read-string-all [s]
  (->> s
       (StringReader.)
       (PushbackReader.)
       (partial edn/read {:eof nil})
       (repeatedly)
       (take-while some?)))

(defn decode-route-params [params]
  (medley/map-vals codec/url-decode params))

(defn route-request [request route]
  (some-> (clout/route-matches route request) decode-route-params))

(def get-or-head (comp #{:get :head} :request-method))

(defn route [handler path f]
  (let [route (clout/route-compile path)]
    (fn [request]
      (if-some [params (and (get-or-head request) (route-request request route))]
        (f request params) (handler request)))))

(defn index [_ _]
  (m/sp {:status 200 :body "TODO"}))

(defn style [_ _]
  (m/sp {:status 200
         :headers {"Content-Type" "text/css"}
         :body (apply garden/css (read-string-all (:body (m/? get-style))))}))

(defn favicon [_ _]
  (m/sp {:status 404}))

(defn posts [_ {:keys [id]}]
  (m/sp (let [[pages markdown] (m/? (m/join get-pages (get-markdown id)))]
          {:status  200
           :headers (-> (:headers markdown)
                        (select-keys ["Cache-Control" "ETag"])
                        (merge {"Content-Type" "text/html"}))
           :body    (-> (:body markdown)
                        (md/md-to-html-string)
                        (h/parse)
                        (h/as-hiccup)
                        (highlight-clojure)
                        (with-headers (get (edn/read-string (:body pages)) id {}))
                        (hiccup/html))})))

(defn wrap [handler]
  (fn [request]
    (try (m/? (handler request))
         (catch Throwable e
           (.printStackTrace e)
           {:status  500
            :headers {"Content-Type" "text/plain"}
            :body    "Internal server error"}))))

(def app
  (-> (fn [_] (m/sp {:status 404 :body "nothing to see here."}))
      (route "/posts/:id" posts)
      (route "/favicon" favicon)
      (route "/style" style)
      (route "/" index)
      (wrap)
      (ch/site)))

(defn -main [& _]
  (j/run-jetty #'app {:port (read-env "PORT")}))
