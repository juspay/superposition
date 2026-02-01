-- Set session variables from psql variables before the DO block
-- This works because psql interpolates :'var' in top-level SQL, but not inside $$ strings
SELECT set_config('cleanup.org_pattern', :'org_pattern', false);
SELECT set_config('cleanup.workspace_name', :'workspace_name', false);

DO $$
DECLARE
    org_record RECORD;
    workspace_record RECORD;
    org_count INTEGER := 0;
    workspace_count INTEGER := 0;
    -- Retrieve variables from session config
    v_org_pattern text := current_setting('cleanup.org_pattern');
    v_workspace_name text := current_setting('cleanup.workspace_name');
BEGIN
    RAISE NOTICE '========================================';
    RAISE NOTICE 'Starting cleanup';
    
    IF v_workspace_name IS NOT NULL AND v_workspace_name <> '' THEN
        -----------------------------------------------------
        -- SPECIFIC WORKSPACE CLEANUP MODE
        -----------------------------------------------------
        RAISE NOTICE 'Mode: DELETE SPECIFIC WORKSPACE';
        RAISE NOTICE 'Org Pattern: %', v_org_pattern;
        RAISE NOTICE 'Workspace:   %', v_workspace_name;
        RAISE NOTICE '========================================';

        FOR org_record IN 
            SELECT id, name FROM superposition.organisations 
            WHERE id LIKE v_org_pattern
        LOOP
            RAISE NOTICE 'Checking Org: % (%)', org_record.id, org_record.name;
            
            -- Find specific workspaces
            FOR workspace_record IN 
                SELECT workspace_schema_name, workspace_name 
                FROM superposition.workspaces
                WHERE organisation_id = org_record.id
                AND workspace_name = v_workspace_name
            LOOP
                RAISE NOTICE '  - Dropping schema: % (workspace: %)', 
                    workspace_record.workspace_schema_name, workspace_record.workspace_name;
                
                EXECUTE format('DROP SCHEMA IF EXISTS %I CASCADE', workspace_record.workspace_schema_name);
                
                -- Delete workspace
                DELETE FROM superposition.workspaces 
                WHERE organisation_id = org_record.id 
                AND workspace_name = workspace_record.workspace_name;
                
                RAISE NOTICE '  - Deleted workspace record';
                workspace_count := workspace_count + 1;
            END LOOP;
        END LOOP;

    ELSE
        -----------------------------------------------------
        -- FULL ORG CLEANUP MODE (Original Behavior)
        -----------------------------------------------------
        RAISE NOTICE 'Mode: FULL ORGANIZATION CLEANUP';
        RAISE NOTICE 'Org Pattern: %', v_org_pattern;
        RAISE NOTICE '========================================';
        
        FOR org_record IN 
            SELECT id, name FROM superposition.organisations 
            WHERE id LIKE v_org_pattern
        LOOP
            RAISE NOTICE '';
            RAISE NOTICE 'Organization: % (%)', org_record.id, org_record.name;
            org_count := org_count + 1;
            
            -- Drop workspace schemas
            FOR workspace_record IN 
                SELECT workspace_schema_name, workspace_name 
                FROM superposition.workspaces
                WHERE organisation_id = org_record.id
            LOOP
                RAISE NOTICE '  - Dropping schema: % (workspace: %)', 
                    workspace_record.workspace_schema_name, workspace_record.workspace_name;
                EXECUTE format('DROP SCHEMA IF EXISTS %I CASCADE', workspace_record.workspace_schema_name);
                workspace_count := workspace_count + 1;
            END LOOP;
            
            -- Delete workspaces
            DELETE FROM superposition.workspaces WHERE organisation_id = org_record.id;
            RAISE NOTICE '  - Deleted workspaces from superposition.workspaces';
            
            -- Delete organization
            DELETE FROM superposition.organisations WHERE id = org_record.id;
            RAISE NOTICE '  - Deleted organization from superposition.organisations';
        END LOOP;
        
        RAISE NOTICE '  Organizations deleted: %', org_count;
    END IF;
    
    RAISE NOTICE '';
    RAISE NOTICE '========================================';
    RAISE NOTICE 'Cleanup Summary:';
    RAISE NOTICE '  Workspace schemas dropped/deleted: %', workspace_count;
    IF org_count > 0 THEN
        RAISE NOTICE '  Organizations deleted: %', org_count;
    END IF;
    RAISE NOTICE '========================================';
END $$;
