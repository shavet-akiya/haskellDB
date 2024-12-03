#ifndef MONGO_HS
#define MONGO_HS

#include <mongoc/mongoc.h>

mongoc_client_t *initialize_mongo(const char *uri, bson_error_t *error);
void destroy_mongo(mongoc_client_t *client);

#endif
