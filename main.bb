#!/usr/bin/env bb

(require '[clojure.string :as str])
(require '[babashka.fs :as fs])
(require '[clojure.java.io :as io])
(require '[babashka.http-client :as http])
(require '[taoensso.timbre :as log])
(import '[java.time Instant])

(set! *warn-on-reflection* true)

(defn rfc1123-datetime-formatted
  [timestamp]
  (->
   (Instant/ofEpochSecond timestamp)
   (.atZone (java.time.ZoneId/of "UTC"))
   (.format  java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)))

(comment
  (rfc1123-datetime-formatted 1678886400))

(defn replace-and-char
  "Replace & to &amp;"
  [s]
  (str/replace s "&" "&amp;"))

(comment
  (replace-and-char "https://abc.xyz?foo=1&bar=2&baz=3"))

(defn write-cdata
  "Write [data] wrapped by [CDATA] into [writer]"
  [writer data]
  (doto writer
    (.write "<![CDATA[ ")
    (.write data)
    (.write " ]]>")))

(def base-header {:accept "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
                  :accept-encoding "gzip, deflate, br"
                  :accept-language "zh-CN,zh-HK;q=0.9,zh;q=0.8"
                  :cache-control "max-age=0"
                  :user-agent "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Mobile Safari/537.36"})

(defn ^String url->html
  "Fetch html content by the given url"
  [^String url]
  (log/info "Fetching" url)
  (-> (http/get url {:throw true :header base-header})
      :body))

(defmulti extract (fn [tag html] tag))

(defmethod extract :default
  [_ _]
  (throw (ex-info "Unimplemented default method for multi-methods [extract]" {})))

(defmethod extract :title
  [_ html]
  (let [re #"<h2>([\s\S]+?)</h2>"]
    (->> html (re-seq re) first last str/trim)))

(defmethod extract :description
  [_ html]
  (let [re #"<div class=\"text ellipsis-4 ivu-mt\">\s*<p>([\s\S]+?)</p>"]
    (->> html (re-seq re) first last str/trim)))

(defmethod extract :cover
  [_ html]
  (let [re #"<div class=\"bgImg\">\s*<img src=\"(.+?)\""]
    (->> html (re-seq re) first last str/trim)))

(defmethod extract :items
  [_ html]
  (let [re #"<button type=\"button\" onclick=\"preventJump\(event,'(.+?)','(.+?)'\)\""]
    (some->> html
             (re-seq re)
             (map rest))))

(defn write-items
  [writer items cover]
  (let [timestamp (volatile!
                   (.getEpochSecond (Instant/now)))]

    (doseq [[item-url item-title] items]
      (doto writer
        (.write "<item>")
        (.write "<guid>")
        (.write "")
        (.write "</guid>")
        (.write "<title>")
        (write-cdata item-title)
        (.write "</title>")
        (.write "<description>")
        (write-cdata item-title)
        (.write "</description>")
        (.write "<pubDate>")
        (.write (rfc1123-datetime-formatted @timestamp))
        (.write "</pubDate>")
        (.write "<itunes:duration>")
        (.write "")
        (.write "</itunes:duration>")
        (.write (format "<itunes:image href=\"%s\"/>" (or (some-> cover replace-and-char) "")))
        (.write (format "<enclosure url=\"%s\" type=\"audio/mp3\"/>" (replace-and-char item-url)))
        (.write "</item>"))

      (vreset! timestamp (inc @timestamp))))

  writer)

(defn gen-rss
  [url]

  (let [html (url->html url)
        title (extract :title html)
        description (extract :description html)
        cover (extract :cover html)
        items (extract :items html)
        out-path (str title ".xml")]

    (log/info "Writing" out-path)

    (with-open [writer (io/writer out-path)]

      (doto writer
        (.write "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
        (.write "<rss xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\" xmlns:atom=\"http://www.w3.org/2005/Atom\" version=\"2.0\">")
        (.write "<channel>")
        (.write "<title>")
        (write-cdata title)
        (.write "</title>")
        (.write "<description>")
        (write-cdata description)
        (.write "</description>")
        (.write "<link>")
        (.write (replace-and-char url))
        (.write "</link>")
        (.write (or (some->>
                     cover
                     replace-and-char
                     (format "<itunes:image href=\"%s\"/>")) ""))

        (write-items items cover)

        (.write "</channel>")
        (.write "</rss>")
        (.flush))

      nil)))

;; === Main ===

(doseq [url (->> (fs/read-all-lines "url.txt")
                 sort
                 distinct)]
  (gen-rss url))

