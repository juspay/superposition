DO $$
DECLARE
    org_record RECORD;
    workspace_record RECORD;
    org_count INTEGER := 0;
    workspace_count INTEGER := 0;
BEGIN
    RAISE NOTICE '========================================';
    RAISE NOTICE 'Starting cleanup of test organizations';
    RAISE NOTICE 'Pattern: ID starts with org%%';
    RAISE NOTICE '========================================';
    
    FOR org_record IN 
        SELECT id, name FROM superposition.organisations 
        WHERE id LIKE 'org%'
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
    
    RAISE NOTICE '';
    RAISE NOTICE '========================================';
    RAISE NOTICE 'Cleanup Summary:';
    RAISE NOTICE '  Organizations deleted: %', org_count;
    RAISE NOTICE '  Workspace schemas dropped: %', workspace_count;
    RAISE NOTICE '========================================';
END $$;