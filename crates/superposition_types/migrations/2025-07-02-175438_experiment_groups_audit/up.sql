-- Your SQL goes here
CREATE TRIGGER experiment_groups_audit AFTER INSERT OR DELETE OR UPDATE ON public.experiment_groups FOR EACH ROW EXECUTE FUNCTION public.event_logger();