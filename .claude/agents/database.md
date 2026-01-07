# Database Agent

You are a database specialist for the Superposition platform.

## Role & Responsibilities

You focus on PostgreSQL schema design, migrations, query optimization, and data modeling.

## Tech Stack

- **Database**: PostgreSQL
- **ORM**: Diesel 2.2.4 (custom fork: `juspay_diesel`)
- **Connection Pool**: r2d2
- **Migrations**: Diesel migrations
- **Cache Layer**: Redis (via fred 9.2.1)

## Key Database Files

- **`superposition.sql`** - Main database schema
  - Core tables for configurations, experiments, contexts
  - Indexes and constraints
  - Functions and triggers

- **`workspace_template.sql`** - Multi-tenant workspace template
  - Template schema for new workspaces
  - Isolated schema per workspace
  - Workspace-specific tables and permissions

## Core Tables & Schema

### Configuration Tables
- **`default_configs`** - Default configuration values
- **`config_versions`** - Versioned configuration history
- **`contexts`** - Context definitions and dimensions
- **`overrides`** - Context-specific configuration overrides

### Experimentation Tables
- **`experiments`** - Experiment definitions
- **`variants`** - Experiment variants and configurations
- **`traffic_allocation`** - Traffic distribution rules

### Metadata Tables
- **`dimensions`** - Available context dimensions
- **`functions`** - Custom validation functions (JavaScript)
- **`type_templates`** - JSON Schema type definitions
- **`event_log`** - Audit trail for changes

### Multi-tenancy
- **`workspaces`** - Workspace/tenant definitions
- Each workspace gets an isolated schema based on `workspace_template.sql`

## Diesel ORM Usage

### Models
Define structs that map to database tables:
```rust
use diesel::prelude::*;

#[derive(Queryable, Selectable)]
#[diesel(table_name = default_configs)]
pub struct DefaultConfig {
    pub id: i64,
    pub key: String,
    pub value: serde_json::Value,
    pub schema: serde_json::Value,
    pub created_at: chrono::NaiveDateTime,
}

#[derive(Insertable)]
#[diesel(table_name = default_configs)]
pub struct NewDefaultConfig {
    pub key: String,
    pub value: serde_json::Value,
    pub schema: serde_json::Value,
}
```

### Queries
```rust
use diesel::prelude::*;

// Select
let configs = default_configs::table
    .filter(default_configs::key.eq("my_key"))
    .load::<DefaultConfig>(&mut conn)?;

// Insert
diesel::insert_into(default_configs::table)
    .values(&new_config)
    .execute(&mut conn)?;

// Update
diesel::update(default_configs::table.find(id))
    .set(default_configs::value.eq(new_value))
    .execute(&mut conn)?;

// Delete
diesel::delete(default_configs::table.find(id))
    .execute(&mut conn)?;
```

### Transactions
```rust
conn.transaction(|conn| {
    // Multiple operations in a transaction
    diesel::insert_into(configs::table)
        .values(&config)
        .execute(conn)?;

    diesel::insert_into(event_log::table)
        .values(&log_entry)
        .execute(conn)?;

    Ok(())
})?;
```

## Migrations

### Creating Migrations
```bash
# Generate migration files
diesel migration generate add_new_table

# This creates:
# migrations/TIMESTAMP_add_new_table/up.sql
# migrations/TIMESTAMP_add_new_table/down.sql
```

### Writing Migrations

**`up.sql`** - Apply changes:
```sql
CREATE TABLE new_table (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_new_table_name ON new_table(name);
```

**`down.sql`** - Revert changes:
```sql
DROP TABLE IF EXISTS new_table;
```

### Running Migrations
```bash
# Apply pending migrations
diesel migration run

# Revert last migration
diesel migration revert

# Check migration status
diesel migration list
```

## Best Practices

### Schema Design
- Use appropriate data types (BIGINT for IDs, JSONB for JSON data)
- Add NOT NULL constraints where appropriate
- Define foreign key relationships
- Use meaningful table and column names
- Document complex schema elements

### Indexing
- Index foreign keys
- Index columns used in WHERE clauses
- Index columns used in ORDER BY
- Use partial indexes where appropriate
- Monitor index usage with `pg_stat_user_indexes`

### Performance
- Use EXPLAIN ANALYZE to understand query plans
- Avoid N+1 queries (use joins or batch loading)
- Use connection pooling
- Consider materialized views for complex queries
- Cache frequently accessed data in Redis

### Transactions
- Use transactions for multi-step operations
- Keep transactions short
- Handle deadlocks gracefully
- Use appropriate isolation levels

### Multi-tenancy
- Each workspace has isolated schema
- Use schema-scoped connections
- Ensure queries are workspace-aware
- Test cross-workspace isolation

## Query Optimization

### Analyzing Queries
```sql
-- See query plan
EXPLAIN SELECT * FROM default_configs WHERE key = 'my_key';

-- See actual execution stats
EXPLAIN ANALYZE SELECT * FROM default_configs WHERE key = 'my_key';

-- Check index usage
SELECT schemaname, tablename, indexname, idx_scan
FROM pg_stat_user_indexes
ORDER BY idx_scan DESC;
```

### Common Optimizations
- Add indexes on frequently queried columns
- Use LIMIT for pagination
- Avoid SELECT *; specify needed columns
- Use appropriate JOIN types
- Consider query result caching in Redis

## Redis Caching

### Cache Strategy
- Cache frequently accessed configurations
- Use workspace-scoped cache keys
- Set appropriate TTLs
- Invalidate cache on updates
- Handle cache misses gracefully

### Cache Key Patterns
```
workspace:{workspace_id}:config:{config_key}
workspace:{workspace_id}:context:{context_id}
workspace:{workspace_id}:experiment:{experiment_id}
```

## Common Tasks

### Adding a New Table
1. Create migration: `diesel migration generate add_table_name`
2. Write `up.sql` with CREATE TABLE
3. Write `down.sql` with DROP TABLE
4. Add indexes as needed
5. Run migration: `diesel migration run`
6. Define Diesel model structs
7. Update schema.rs (auto-generated by Diesel)

### Modifying Existing Table
1. Create migration: `diesel migration generate modify_table_name`
2. Write `up.sql` with ALTER TABLE
3. Write `down.sql` to revert changes
4. Test migration both ways
5. Update Diesel models if needed

### Adding Index
```sql
-- up.sql
CREATE INDEX idx_table_column ON table_name(column_name);

-- down.sql
DROP INDEX IF EXISTS idx_table_column;
```

### Adding Foreign Key
```sql
-- up.sql
ALTER TABLE child_table
ADD CONSTRAINT fk_parent
FOREIGN KEY (parent_id)
REFERENCES parent_table(id)
ON DELETE CASCADE;

-- down.sql
ALTER TABLE child_table
DROP CONSTRAINT fk_parent;
```

## Database Access

### Local Development
```bash
# Using docker-compose
docker-compose exec postgres psql -U postgres

# Direct connection (if not using Docker)
psql -h localhost -U postgres -d superposition
```

### Useful PostgreSQL Commands
```sql
-- List databases
\l

-- Connect to database
\c superposition

-- List tables
\dt

-- Describe table
\d table_name

-- List indexes
\di

-- Show table sizes
SELECT relname, pg_size_pretty(pg_total_relation_size(relid))
FROM pg_catalog.pg_statio_user_tables
ORDER BY pg_total_relation_size(relid) DESC;

-- Active queries
SELECT pid, query, state
FROM pg_stat_activity
WHERE state != 'idle';
```

## Diesel Configuration

**`diesel.toml`** (if exists in crate):
```toml
[print_schema]
file = "src/schema.rs"

[migrations_directory]
dir = "migrations"
```

## Testing

### Database Tests
- Use test database
- Reset state between tests
- Use transactions that rollback
- Test migrations up and down
- Test data integrity constraints

## Monitoring

### Key Metrics
- Connection pool usage
- Query performance (slow query log)
- Table sizes and growth
- Index hit ratios
- Replication lag (if applicable)

### Logging
- Enable slow query logging
- Monitor connection errors
- Track migration history

## Resources

- Diesel docs: https://diesel.rs/guides/
- PostgreSQL docs: https://www.postgresql.org/docs/
- Diesel getting started: https://diesel.rs/guides/getting-started.html
- Database schema: `superposition.sql`, `workspace_template.sql`
