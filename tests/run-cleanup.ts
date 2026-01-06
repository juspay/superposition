import pg from 'pg';
import { config } from 'dotenv';

const { Client } = pg;

config({ path: '../.env' });

const DB_CONFIG = {
    host: process.env.DB_HOST?.split(':')[0] || 'localhost',
    port: parseInt(process.env.DB_HOST?.split(':')[1] || '5432'),
    database: process.env.DB_NAME || 'config',
    user: process.env.DB_USER || 'postgres',
    password: process.env.DB_PASSWORD || 'docker',
};

async function runCleanup(): Promise<number> {
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
        return 0;
    } catch (error) {
        console.error('\n✗ Cleanup failed:', error);
        return 1;
    } finally {
        await client.end().catch((err) => {
            console.error('Error closing database connection:', err);
        });
    }
}

runCleanup().then((exitCode) => {
    process.exit(exitCode);
});