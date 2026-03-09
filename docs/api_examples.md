# Casbin Management API Examples

This document provides sample `curl` commands for the Casbin management API endpoints.

**Note:** All endpoints are prefixed with `/auth/casbin` and require a schema name, which is passed via the `X-Schema-Name` header in these examples. The base URL is assumed to be `http://localhost:8080`.

---

## Policy Management

### Add a policy

Adds a new policy rule.

`POST /auth/casbin/policies`

```bash
curl -X POST http://localhost:8080/auth/casbin/policies \
-H "Content-Type: application/json" \
-H "X-Schema-Name: my_workspace" \
-d '{
    "sub": "user1",
    "obj": "payments:payment_123",
    "act": "read",
    "attr": "amount"
}'
```

_Note: The `obj` field for `Resource` is assumed to be a string. Given `key_match3` in your `model.conf`, a format like `resource_type:resource_id` is a common pattern._

### Remove a policy

Removes an existing policy rule.

`DELETE /auth/casbin/policies`

```bash
curl -X DELETE http://localhost:8080/auth/casbin/policies \
-H "Content-Type: application/json" \
-H "X-Schema-Name: my_workspace" \
-d '{
    "sub": "user1",
    "obj": "payments:payment_123",
    "act": "read",
    "attr": "amount"
}'
```

### Get all policies for a schema

Lists all policy rules for the given schema.

`GET /auth/casbin/policies`

```bash
curl -X GET http://localhost:8080/auth/casbin/policies \
-H "X-Schema-Name: my_workspace"
```

---

## Role Management (Grouping Policies)

### Add a role assignment (grouping policy)

Assigns a role to a user.

`POST /auth/casbin/roles`

```bash
curl -X POST http://localhost:8080/auth/casbin/roles \
-H "Content-Type: application/json" \
-H "X-Schema-Name: my_workspace" \
-d '{
    "user": "user1",
    "role": "admin"
}'
```

### Remove a role assignment (grouping policy)

Removes a role from a user.

`DELETE /auth/casbin/roles`

```bash
curl -X DELETE http://localhost:8080/auth/casbin/roles \
-H "Content-Type: application/json" \
-H "X-Schema-Name: my_workspace" \
-d '{
    "user": "user1",
    "role": "admin"
}'
```

### Get all role assignments

Lists all role assignments (grouping policies).

`GET /auth/casbin/roles`

```bash
curl -X GET http://localhost:8080/auth/casbin/roles \
-H "X-Schema-Name: my_workspace"
```

### Get roles for a user

Lists all roles assigned to a specific user.

`GET /auth/casbin/users/{user}/roles`

```bash
curl -X GET http://localhost:8080/auth/casbin/users/user1/roles \
-H "X-Schema-Name: my_workspace"
```

### Get users for a role

Lists all users that have a specific role.

`GET /auth/casbin/roles/{role}/users`

```bash
curl -X GET http://localhost:8080/auth/casbin/roles/admin/users \
-H "X-Schema-Name: my_workspace"
```
