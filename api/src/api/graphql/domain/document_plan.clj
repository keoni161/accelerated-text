(ns api.graphql.domain.document-plan
  (:require [api.graphql.translate.document-plan :as translate-dp]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [data.entities.document-plan :as dp]))

(defn- resolve-as-not-found-document-plan [id]
  (resolve-as nil {:message (format "Cannot find document plan with id `%s`." id)}))

(defn get-document-plan [_ {:keys [id]} _]
  (if-let [document-plan (dp/get-document-plan id)]
    (resolve-as (translate-dp/dp->schema document-plan))
    (resolve-as-not-found-document-plan id)))

(defn delete-document-plan [_ {:keys [id]} _]
  (dp/delete-document-plan id)
  (resolve-as true))

(defn list-document-plans [_ {:keys [offset limit] :or {offset 0 limit 20}} _]
  (let [items (dp/list-document-plans)]
    (resolve-as {:items      (->> items
                                  (drop offset)
                                  (take limit)
                                  (map translate-dp/dp->schema))
                 :limit      limit
                 :offset     offset
                 :totalCount (count items)})))

(defn add-document-plan [_ args _]
  (->> (translate-dp/schema->dp args)
       (dp/add-document-plan)
       (translate-dp/dp->schema)
       (resolve-as)))

(defn update-document-plan [_ {:keys [id] :as args} _]
  (if-let [document-plan (dp/update-document-plan id (translate-dp/schema->dp args))]
    (resolve-as (translate-dp/dp->schema document-plan))
    (resolve-as-not-found-document-plan id)))
