-- Your SQL goes here
CREATE TRIGGER experiments_audit
AFTER INSERT OR UPDATE OR DELETE ON cac_v1.experiments
FOR EACH ROW EXECUTE PROCEDURE cac_v1.event_logger();
