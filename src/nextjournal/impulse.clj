(ns nextjournal.impulse
  (:require [duratom.core :refer [duratom]]
            [hiccup.page :refer [html5 include-css include-js]]
            [hiccup2.core :as h]
            [nextjournal.garden-email :as garden-email]
            [nextjournal.garden-id :as garden-id]
            [org.httpkit.server :as server]
            [reitit.core :as r]
            [reitit.ring :as rr]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.session.memory :refer [memory-store]]
            [ring.util.codec :as codec]
            [ring.util.response :refer [response response? not-found content-type]]))

(defonce
  ^{:doc "An atom containing in-memory session info."}
  session (atom {}))

(defn page [opts & contents]
  (if-not (map? opts)
    (apply page {} opts contents)
    (let [{:keys [title lang charset head]
           :or {title "Impulse power!"
                lang "en"
                charset "utf-8"}} opts]
      (html5 {:lang lang}
             (into [:head
                    [:title title]
                    [:meta {:charset charset}]
                    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
                    [:meta {:name "color-scheme" :content "light dark"}]
                    (include-js "htmx.org@2.0.2.js")
                    (include-css "pico.min.css")]
                   head)
             [:body
              ;;TODO move to garden-email?
              (when garden-email/dev-mode?
                [:section.container
                 [:nav [:ul [:li [:a {:href garden-email/outbox-url} "sent emails"]]]]
                 [:hr]])
              (into [:main.container] contents)]))))

(def ^{:doc "An atom backed by persistent storage."}
  state
  (duratom :local-file
           :file-path (str (System/getenv "GARDEN_STORAGE") "/duratom.edn")
           :commit-mode :sync
           :init {}))

(defn wrap-hiccup
  "Middleware that interprets anything enclosed in a vector as Hiccup, which it converts to an HTML string with an appropriate response envelop."
  [f]
  (fn [req]
    (let [res (f req)]
      (cond (response? res) res
            (string? res) (-> res response (content-type "text/html"))
            (vector? res) (-> (h/html res) str response (content-type "text/html"))
            :else res))))

(defonce
  ^{:doc "An atom containing the last request that was received by the system. Extremely useful for writing and debugging handler functions."}
  last-request
  (atom {}))
(defn wrap-save-request
  "Middleware that saves the most recent request to the `last-request` atom."
  [f]
  (fn [req]
    (reset! last-request req)
    (f req)))

(defn handle-not-found
  "Handler to serve a 404."
  [req]
  (not-found "Not found"))

(defn swap-in!
  "Use swap! with update-in, but returns the updated (nested) value instead of the whole atom. This is especially useful when updating the `state` atom at some path."
  [atom ks f & args]
  (get-in (apply swap! atom update-in ks f args) ks))

(defonce
  ^{:doc "An atom containing the currently running webserver (or nil)."}
  !server (atom nil))

(defn stop!
  "Stop the currently running webserver."
  []
  (if-let [s @!server]
    (do (server/server-stop! s)
        (reset! !server nil))
    (throw (ex-info "Server not running" {}))))

(defn symbol->path
  "Turn a symbol into a urlsafe path"
  [symbol]
  ;;FIXME
  (str "/" symbol))

(defonce !routes (atom {}))

(defn request? [r]
  (and (map? r)
       (contains? r :body)
       (contains? r ::r/router)
       (contains? r ::r/match)))

(def ^:dynamic *req* nil)

(defn wrap-*req* [handler]
  (fn [req]
    (binding [*req* req]
      (handler req))))

(defmacro defhandler
  "Create a request handler
  (defhandler foo [a b] (str a b))
    {:hx-post (foo 1 2)}"
  [name method args & body]
  (if-not (keyword? method)
    `(defhandler ~name :post ~method ~args ~@body)
    (let [path (symbol->path name)
          req (gensym "req")
          definition (cond->
                         `(defn ~name
                            ([~req] (if (request? ~req)
                                      (let [{:strs ~args} (:query-params ~req)]
                                        ~@body)
                                      (str ~path "?" (codec/form-encode ~{(keyword (first args)) req})))))
                       (= 0 (count args)) (concat `((~args ~path)))
                       (< 1 (count args)) (concat `((~args (str ~path "?" (codec/form-encode ~(into {} (for [arg args] [(keyword arg) arg]))))))))]
      `(let [handler# ~definition]
         (swap! !routes assoc ~path {~method handler#})
         handler#))))

(defn start!
  "Start a webserver. If a server is already running, restarts it.

  Takes reitit `routes` and `opts` for `http-kit/run-server`.

  When `routes` is a var, resolves it on every request, to allow interactive development."
  [routes opts]
  (let [reitit-opts {:middleware [wrap-content-type
                                  (fn [h] (wrap-session h {:store (memory-store session)}))
                                  (fn [h] (wrap-resource h "/"))
                                  wrap-params
                                  garden-id/wrap-auth
                                  garden-email/wrap-with-email
                                  wrap-hiccup
                                  wrap-*req*
                                  wrap-save-request]}]
    (when-not (nil? @!server)
      (stop!))
    (reset! !server
            (server/run-server (if (var? routes)
                                 (rr/reloading-ring-handler
                                  #(rr/ring-handler
                                    (rr/router (concat (into [] @!routes) @routes))
                                    handle-not-found
                                    reitit-opts))
                                 (rr/ring-handler
                                  (rr/router (concat (into [] @!routes) @!routes routes))
                                  handle-not-found
                                  reitit-opts))
                               (merge {:port 7777}
                                      opts
                                      {:legacy-return-value? false})))))
