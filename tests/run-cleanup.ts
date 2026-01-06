import pg from 'pg';

const { Client } = pg;

const DB_CONFIG = {
    host: 'localhost',
    port: 5432,
    database: 'config',
    user: 'postgres',
    password: 'docker',
};

async function runCleanup(): Promise<void> {
    const client = new Client(DB_CONFIG);
    
    try {
        console.log('Connecting to PostgreSQL...');
        await client.connect();
        console.log('Connected successfully');
        
        const sqlPath = new URL('./cleanup.sql', import.meta.url).pathname;
        const sqlContent = await Bun.file(sqlPath).text();
        
        console.log('\nExecuting cleanup script...\n');
        
        client.on('notice', (msg) => {
            console.log(msg.message);
        });
        
        await client.query(sqlContent);
        
        console.log('\n✓ Cleanup completed successfully');
        process.exit(0);
    } catch (error) {
        console.error('\n✗ Cleanup failed:', error);
        process.exit(1);
    } finally {
        await client.end().catch((err) => {
            console.error('Error closing database connection:', err);
        });
    }
}

runCleanup();