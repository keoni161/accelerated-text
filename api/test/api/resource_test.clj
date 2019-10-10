(ns api.resource-test
  (:require [clojure.test :refer [deftest is]]
            [api.resource :as resource]
            [jsonista.core :as json])
  (:import (java.io BufferedInputStream ByteArrayInputStream)))

(deftest decode-input
  (let [request {:httpMethod            "POST",
                 :queryStringParameters nil,
                 :headers               {:referer         "http://localhost:8080/",
                                         :user-agent      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:69.0) Gecko/20100101 Firefox/69.0",
                                         :host            "0.0.0.0:3001",
                                         :content-length  "113",
                                         :accept-encoding "gzip, deflate",
                                         :content-type    "application/json",
                                         :origin          "http://localhost:8080",
                                         :connection      "keep-alive",
                                         :accept-language "en-US,en;q=0.5",
                                         :accept          "*/*",
                                         :dnt             "1"}
                 :body                  "{\"documentPlanId\":\"070cd048-b2b9-4890-bdcc-4420ac449c86\",\"readerFlagValues\":{},\"dataId\":\"example-user/books.csv\"}",
                 :pathParameters        {}}
        input-stream (BufferedInputStream. (ByteArrayInputStream. (.getBytes (json/write-value-as-string request))))]
    (is (some? (resource/decode-input input-stream true)))))