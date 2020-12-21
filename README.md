# haskell-begining

```
curl --location --request GET 'http://localhost:8080/users'
```


```
curl --location --request POST 'http://localhost:8080/users' \
--header 'Content-Type: application/json' \
--data-raw '{
    "user_name": "User Name", 
    "user_age": 25,  
    "user_email": "otheruser@mail.com"
}'
```
