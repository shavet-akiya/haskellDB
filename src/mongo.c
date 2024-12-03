#include "mongo.h"

mongoc_client_t *initialize_mongo(const char *uri, bson_error_t *error) {
  mongoc_init();
  mongoc_client_t *client = mongoc_client_new(uri);
  if (!client) {
    return NULL;
  }
  return client;
}

void destro_mongo(mongoc_client_t *client) {
  if (client) {
    mongoc_client_destroy(client);
  }

  mongoc_cleanup();
}
