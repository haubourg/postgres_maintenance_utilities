--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5 (Debian 13.5-1.pgdg110+1)
-- Dumped by pg_dump version 13.3 (Ubuntu 13.3-1.pgdg20.04+1)

--
-- Name: admin_pg; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA admin_pg;

--
-- Name: SCHEMA admin_pg; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA admin_pg IS 'schema de stockage des vues et fonctions de maintenance postgres (data et infra)';


SET SEARCH_PATH TO admin_pg;
--
-- Name: active_queries(); Type: FUNCTION; Schema: admin_pg; Owner: -
--

CREATE FUNCTION active_queries() RETURNS TABLE(pid integer, age interval, usename name, state text, q text)
    LANGUAGE plpgsql
    AS $$
        BEGIN
                RETURN QUERY SELECT pas.pid, age(clock_timestamp(), pas.query_start), pas.usename, pas.state, substring(pas.query, 0, 38) as q
                FROM pg_stat_activity pas
                WHERE query != '<IDLE>' AND query NOT ILIKE '%pg_stat_activity%' 
                ORDER BY query_start desc;
        END;
$$;


--
-- Name: drop_unused_index(); Type: PROCEDURE; Schema: admin_pg; Owner: -
--

CREATE PROCEDURE drop_unused_index()
    LANGUAGE plpgsql
    AS $$
DECLARE
	r RECORD;
BEGIN
	FOR r IN 
		(SELECT  quote_ident(schemaname) || '.' || quote_ident(indexname) AS indexname FROM index_safely_droppable
		 WHERE schemaname NOT IN ('tmp_adr'))
	LOOP
		RAISE INFO 'DROPING INDEX: %', r.indexname;
		EXECUTE 'DROP INDEX IF EXISTS ' || r.indexname;
		COMMIT;
	END LOOP;	
END
$$;


--
-- Name: PROCEDURE drop_unused_index(); Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON PROCEDURE drop_unused_index() IS 'fonction de suppressions des indexes. USAGE: `CALL drop_unused_index();` ';


--
-- Name: func_change_tablespace(text, text); Type: PROCEDURE; Schema: admin_pg; Owner: -
--

CREATE PROCEDURE func_change_tablespace(sourceschema text, newtablespace text)
    LANGUAGE plpgsql
    AS $$
declare
    tblnames CURSOR FOR
        select c.relname as tablename from pg_class c 
        join pg_namespace nsp on nsp.oid = c.relnamespace
        left outer join pg_tablespace tbsp on c.reltablespace = tbsp.oid -- default tablespace doesn't exists
        where 
        nsp.nspname=quote_ident(sourceschema)
        -- only tables
        and relkind = 'r'
        -- that are not already in the target tablespace
        and (tbsp.spcname != newtablespace OR tbsp.spcname IS null); 

    sql_instruction text;
    idx_sql_instruction text; 
    matv_sql_instruction text;
    num_relations integer;

--   indexnames CURSOR FOR
--     SELECT viewname FROM pg_views
--     WHERE schemaname = pschem;

    indexnames CURSOR FOR
        select c.relname as indexname from pg_class c 
        join pg_namespace nsp on nsp.oid = c.relnamespace
        left outer join pg_tablespace tbsp on c.reltablespace = tbsp.oid -- default tablespace doesn't exists
        where 
        nsp.nspname=quote_ident(sourceschema)
        -- only tables
        and relkind = 'i'
        -- that are not already in the target tablespace
        and (tbsp.spcname != newtablespace OR tbsp.spcname IS null); 

    matviewnames CURSOR FOR
        select c.relname as matviewname from pg_class c 
        join pg_namespace nsp on nsp.oid = c.relnamespace
        left outer join pg_tablespace tbsp on c.reltablespace = tbsp.oid -- default tablespace doesn't exists
        where 
        nsp.nspname=quote_ident(sourceschema)
        -- only tables
        and relkind = 'm'
        -- that are not already in the target tablespace
        and (tbsp.spcname != newtablespace OR tbsp.spcname IS null); 

BEGIN

-- Checks if target tablespace exists
  IF NOT (select exists(select 1 from pg_tablespace where spcname = newtablespace)) THEN
    RAISE EXCEPTION 'Target tablespace does not exists %.', newtablespace;
  END IF;

-- Checks the number of relations to handle
    select count(*) INTO num_relations from pg_class c 
    join pg_namespace nsp on nsp.oid = c.relnamespace
    left outer join pg_tablespace tbsp on c.reltablespace = tbsp.oid -- default tablespace doesn't exists
    where 
    nsp.nspname=quote_ident(sourceschema)
    -- only tables
    and relkind in ('r' , 'i', 'm')
    -- that are not already in the target tablespace
    and (tbsp.spcname != newtablespace OR tbsp.spcname IS null);

    IF num_relations = 0 or num_relations is NULL THEN
        RAISE EXCEPTION 'No relation to move from %', sourceschema;
    ELSE 
        RAISE NOTICE '% relations to move from %', num_relations, sourceschema;
    END IF;


-- do the work 

    FOR stmt IN tblnames LOOP
        sql_instruction = 'alter TABLE ' || quote_ident(sourceschema) || '.' || quote_ident(stmt.tablename) || ' SET TABLESPACE ' || newtablespace || ';' ;
        RAISE NOTICE  USING MESSAGE = sql_instruction;
        EXECUTE sql_instruction;
        COMMIT; 
    END LOOP;
    
    FOR istmt IN indexnames LOOP
        idx_sql_instruction = 'ALTER INDEX ' || quote_ident(sourceschema) || '.' || quote_ident(istmt.indexname) || ' SET TABLESPACE ' || newtablespace || ';' ;
        RAISE NOTICE  USING MESSAGE = idx_sql_instruction;
        EXECUTE idx_sql_instruction;
        COMMIT;
    END LOOP;

    FOR matviewnames IN matviewnames LOOP
        matv_sql_instruction = 'ALTER MATERIALIZED VIEW ' || quote_ident(sourceschema) || '.' || quote_ident(matviewnames.matviewname) || ' SET TABLESPACE ' || newtablespace || ';' ;
        RAISE NOTICE  USING MESSAGE = matv_sql_instruction;
        EXECUTE matv_sql_instruction;
        COMMIT;
    END LOOP;



    RAISE NOTICE '% relations moved from % to %', num_relations, sourceschema, newtablespace;

--   FOR matstmt IN matviewnames LOOP
--     EXECUTE 'alter VIEW ' || pschem || '.' || stmt.viewname || ' owner to ' || newowner || ';';
--   END LOOP;
--   FOR stmt IN funcnames LOOP
--     EXECUTE 'alter FUNCTION ' || pschem || '.' || stmt.name || '(' ||  stmt.params || ') owner to ' || newowner || ';';
--   END LOOP;

END;

$$;


--
-- Name: PROCEDURE func_change_tablespace(sourceschema text, newtablespace text); Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON PROCEDURE func_change_tablespace(sourceschema text, newtablespace text) IS 'Batch change tablespace of a schema. Ex: select func_change_tablespace(''gorenove_compile_v04'', ''slow2_tablespace'')';


--
-- Name: func_geocode_fts_ban(text, integer); Type: FUNCTION; Schema: admin_pg; Owner: -
--

CREATE FUNCTION func_geocode_fts_ban(sql_source text, sql_ban_limit integer DEFAULT 1) RETURNS TABLE(input_string text, ts_rank real, label text)
    LANGUAGE plpgsql
    AS $$

DECLARE 

r record;
my_ts_query tsquery;

BEGIN
	FOR r IN 
		select id, numero_rue , type_voie, nom_rue, commune,  code_postal, code_insee_commune , tv016_departement_id::text  AS departement
		from adedpe202006.td001_dpe WHERE  tv016_departement_id = 38 
		AND numero_rue IS NOT null
		ORDER BY id 
		LIMIT 3

	LOOP
	 	RAISE INFO 'looping on %', quote_literal(concat_ws(' & ', r.numero_rue, r.type_voie,r.nom_rue, r.code_postal, r.commune ) );
    

		SELECT
				to_tsquery('french', 
					quote_literal(
						concat(
							r.numero_rue,
							' & ',
							r.type_voie,
							' & ',
							-- array_to_string(
							-- 	regexp_split_to_array(trim(r.nom_rue), E'\\s+' ) , 
							-- 	' & ', '*'), 
							r.nom_rue,
							' & ',
							r.code_postal,
							' & ' ,
							r.commune
						)
					  )
					)
					  into my_ts_query;

		RETURN QUERY
			SELECT
				my_ts_query::text as input_string  ,
				--1::real as rank,
				ts_rank_cd(ts , my_ts_query ) AS ts_rank,
				ban.label       
			FROM                                                                                                 
			z_tmp_rhaubourg.ban_data_38  AS ban                 
			WHERE 
			r.departement ::text = ban.departement 
			AND ban.label IS NOT NULL
			ORDER BY ts_rank DESC 
			LIMIT 1 ;

	END LOOP;
	RETURN;

END;

$$;


--
-- Name: FUNCTION func_geocode_fts_ban(sql_source text, sql_ban_limit integer); Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON FUNCTION func_geocode_fts_ban(sql_source text, sql_ban_limit integer) IS 'geocodes a query using ban dataset. Limit argument tells how much results are given back. 1 is the default and will return the best research matching, or null. >1 will produce more lines than input. ';


--
-- Name: histogram(text, text); Type: FUNCTION; Schema: admin_pg; Owner: -
--

CREATE FUNCTION histogram(table_name_or_subquery text, column_name text) RETURNS TABLE(bucket integer, range numrange, freq bigint, bar text)
    LANGUAGE plpgsql
    AS $$
BEGIN
RETURN QUERY EXECUTE format('
  WITH
  source AS (
    SELECT * FROM %s
  ),
  min_max AS (
    SELECT min(%s) AS min, max(%s) AS max FROM source
  ),
  histogram AS (
    SELECT
      width_bucket(%s, min_max.min, min_max.max, 20) AS bucket,
      numrange(min(%s)::numeric, max(%s)::numeric, ''[]'') AS "range",
      count(%s) AS freq
    FROM source, min_max
    WHERE %s IS NOT NULL
    GROUP BY bucket
    ORDER BY bucket
  )
  SELECT
    bucket,
    "range",
    freq::bigint,
    repeat(''*'', (freq::float / (max(freq) over() + 1) * 15)::int) AS bar
  FROM histogram',
  table_name_or_subquery,
  column_name,
  column_name,
  column_name,
  column_name,
  column_name,
  column_name,
  column_name
  );
END
$$;


--
-- Name: FUNCTION histogram(table_name_or_subquery text, column_name text); Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON FUNCTION histogram(table_name_or_subquery text, column_name text) IS 'fonction de génération d''histogrammes en text - source https://faraday.ai/blog/how-to-do-histograms-in-postgresql';


--
-- Name: list_fields(text, text, text); Type: FUNCTION; Schema: admin_pg; Owner: -
--

CREATE FUNCTION list_fields(tableschema text, tablename text, separator text DEFAULT ','::text) RETURNS text
    LANGUAGE sql STABLE
    AS $_$
-- all attributes names, except those belonging primary key
SELECT string_agg(column_name, $3) 
  FROM information_schema.columns
 WHERE table_schema = $1
   AND table_name   = $2
     ;
$_$;


--
-- Name: bloat; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW bloat AS
 SELECT sml.schemaname,
    sml.tablename,
    (sml.reltuples)::bigint AS reltuples,
    (sml.relpages)::bigint AS relpages,
    sml.otta,
    round(
        CASE
            WHEN (sml.otta = (0)::double precision) THEN 0.0
            ELSE ((sml.relpages)::numeric / (sml.otta)::numeric)
        END, 1) AS tbloat,
    (((sml.relpages)::bigint)::double precision - sml.otta) AS wastedpages,
    (sml.bs * ((((sml.relpages)::double precision - sml.otta))::bigint)::numeric) AS wastedbytes,
    pg_size_pretty((((sml.bs)::double precision * ((sml.relpages)::double precision - sml.otta)))::bigint) AS wastedsize,
    sml.iname,
    (sml.ituples)::bigint AS ituples,
    (sml.ipages)::bigint AS ipages,
    sml.iotta,
    round(
        CASE
            WHEN ((sml.iotta = (0)::double precision) OR (sml.ipages = 0)) THEN 0.0
            ELSE ((sml.ipages)::numeric / (sml.iotta)::numeric)
        END, 1) AS ibloat,
        CASE
            WHEN ((sml.ipages)::double precision < sml.iotta) THEN (0)::double precision
            ELSE (((sml.ipages)::bigint)::double precision - sml.iotta)
        END AS wastedipages,
        CASE
            WHEN ((sml.ipages)::double precision < sml.iotta) THEN (0)::double precision
            ELSE ((sml.bs)::double precision * ((sml.ipages)::double precision - sml.iotta))
        END AS wastedibytes,
        CASE
            WHEN ((sml.ipages)::double precision < sml.iotta) THEN pg_size_pretty((0)::bigint)
            ELSE pg_size_pretty((((sml.bs)::double precision * ((sml.ipages)::double precision - sml.iotta)))::bigint)
        END AS wastedisize
   FROM ( SELECT rs.schemaname,
            rs.tablename,
            cc.reltuples,
            cc.relpages,
            rs.bs,
            ceil(((cc.reltuples * (((((rs.datahdr + (rs.ma)::numeric) -
                CASE
                    WHEN ((rs.datahdr % (rs.ma)::numeric) = (0)::numeric) THEN (rs.ma)::numeric
                    ELSE (rs.datahdr % (rs.ma)::numeric)
                END))::double precision + rs.nullhdr2) + (4)::double precision)) / ((rs.bs)::double precision - (20)::double precision))) AS otta,
            COALESCE(c2.relname, '?'::name) AS iname,
            COALESCE(c2.reltuples, (0)::real) AS ituples,
            COALESCE(c2.relpages, 0) AS ipages,
            COALESCE(ceil(((c2.reltuples * ((rs.datahdr - (12)::numeric))::double precision) / ((rs.bs)::double precision - (20)::double precision))), (0)::double precision) AS iotta
           FROM ((((( SELECT foo.ma,
                    foo.bs,
                    foo.schemaname,
                    foo.tablename,
                    ((foo.datawidth + (((foo.hdr + foo.ma) -
                        CASE
                            WHEN ((foo.hdr % foo.ma) = 0) THEN foo.ma
                            ELSE (foo.hdr % foo.ma)
                        END))::double precision))::numeric AS datahdr,
                    (foo.maxfracsum * (((foo.nullhdr + foo.ma) -
                        CASE
                            WHEN ((foo.nullhdr % (foo.ma)::bigint) = 0) THEN (foo.ma)::bigint
                            ELSE (foo.nullhdr % (foo.ma)::bigint)
                        END))::double precision) AS nullhdr2
                   FROM ( SELECT s.schemaname,
                            s.tablename,
                            constants.hdr,
                            constants.ma,
                            constants.bs,
                            sum((((1)::double precision - s.null_frac) * (s.avg_width)::double precision)) AS datawidth,
                            max(s.null_frac) AS maxfracsum,
                            (constants.hdr + ( SELECT (1 + (count(*) / 8))
                                   FROM pg_stats s2
                                  WHERE ((s2.null_frac <> (0)::double precision) AND (s2.schemaname = s.schemaname) AND (s2.tablename = s.tablename)))) AS nullhdr
                           FROM pg_stats s,
                            ( SELECT ( SELECT (current_setting('block_size'::text))::numeric AS current_setting) AS bs,
CASE
 WHEN ("substring"(foo_1.v, 12, 3) = ANY (ARRAY['8.0'::text, '8.1'::text, '8.2'::text])) THEN 27
 ELSE 23
END AS hdr,
CASE
 WHEN (foo_1.v ~ 'mingw32'::text) THEN 8
 ELSE 4
END AS ma
                                   FROM ( SELECT version() AS v) foo_1) constants
                          GROUP BY s.schemaname, s.tablename, constants.hdr, constants.ma, constants.bs) foo) rs
             JOIN pg_class cc ON ((cc.relname = rs.tablename)))
             JOIN pg_namespace nn ON (((cc.relnamespace = nn.oid) AND (nn.nspname = rs.schemaname))))
             LEFT JOIN pg_index i ON ((i.indrelid = cc.oid)))
             LEFT JOIN pg_class c2 ON ((c2.oid = i.indexrelid)))) sml
  WHERE ((((sml.relpages)::double precision - sml.otta) > (0)::double precision) OR (((sml.ipages)::double precision - sml.iotta) > (10)::double precision))
  ORDER BY (sml.bs * ((((sml.relpages)::double precision - sml.otta))::bigint)::numeric) DESC,
        CASE
            WHEN ((sml.ipages)::double precision < sml.iotta) THEN (0)::double precision
            ELSE ((sml.bs)::double precision * ((sml.ipages)::double precision - sml.iotta))
        END DESC;


--
-- Name: columns_always_null; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW columns_always_null AS
 SELECT pg_stats.tablename,
    pg_stats.attname,
    pg_stats.null_frac,
    pg_stats.avg_width,
    pg_stats.n_distinct
   FROM pg_stats
  WHERE ((pg_stats.schemaname ~~ 'gore%'::text) AND (pg_stats.null_frac = (1)::double precision))
  ORDER BY pg_stats.tablename DESC;


--
-- Name: geometry_columns_untyped; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW geometry_columns_untyped AS
 SELECT geometry_columns.f_table_catalog,
    geometry_columns.f_table_schema,
    geometry_columns.f_table_name,
    geometry_columns.f_geometry_column,
    geometry_columns.coord_dimension,
    geometry_columns.srid,
    geometry_columns.type
   FROM public.geometry_columns
  WHERE ((geometry_columns.f_table_name !~~ '%partition%'::text) AND (((geometry_columns.type)::text = 'GEOMETRY'::text) OR (geometry_columns.srid = 0)))
  ORDER BY geometry_columns.f_table_schema, geometry_columns.f_table_name;


--
-- Name: index_duplicates; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW index_duplicates AS
 SELECT pg_size_pretty((sum(pg_relation_size(sub.idx)))::bigint) AS size,
    (array_agg(sub.idx))[1] AS idx1,
    (array_agg(sub.idx))[2] AS idx2,
    (array_agg(sub.idx))[3] AS idx3,
    (array_agg(sub.idx))[4] AS idx4
   FROM ( SELECT (pg_index.indexrelid)::regclass AS idx,
            (((((((((pg_index.indrelid)::text || '
'::text) || (pg_index.indclass)::text) || '
'::text) || (pg_index.indkey)::text) || '
'::text) || COALESCE((pg_index.indexprs)::text, ''::text)) || '
'::text) || COALESCE((pg_index.indpred)::text, ''::text)) AS key
           FROM pg_index) sub
  GROUP BY sub.key
 HAVING (count(*) > 1)
  ORDER BY (sum(pg_relation_size(sub.idx))) DESC;


--
-- Name: VIEW index_duplicates; Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON VIEW index_duplicates IS 'vue des indexes a priori redondants';


--
-- Name: index_safely_droppable; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW index_safely_droppable AS
 SELECT s.schemaname,
    s.relname AS tablename,
    s.indexrelname AS indexname,
    pg_relation_size((s.indexrelid)::regclass) AS index_size,
    pg_size_pretty(sum(pg_relation_size((s.indexrelid)::regclass)) OVER ()) AS taille_totale
   FROM ((pg_stat_user_indexes s
     JOIN pg_index i ON ((s.indexrelid = i.indexrelid)))
     LEFT JOIN pg_depend d ON (((d.objid = i.indexrelid) AND (d.deptype = 'P'::"char"))))
  WHERE ((s.idx_scan = 0) AND (0 <> ALL ((i.indkey)::smallint[])) AND (NOT (EXISTS ( SELECT 1
           FROM pg_constraint c
          WHERE (c.conindid = s.indexrelid)))) AND (pg_get_indexdef(i.indexrelid) !~~ '%gist%'::text) AND (d.objid IS NULL))
  ORDER BY (pg_relation_size((s.indexrelid)::regclass)) DESC;


--
-- Name: VIEW index_safely_droppable; Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON VIEW index_safely_droppable IS 'Vue des indexes supprimables sans risque  - source https://stackoverflow.com/questions/50351169/delete-unused-indexes';


--
-- Name: index_statistics; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW index_statistics AS
 SELECT i.oid AS indexrelid,
    pg_relation_size((i.oid)::regclass) AS index_size,
    pg_size_pretty(pg_relation_size((i.oid)::regclass)) AS index_size_pretty,
    n.nspname AS schemaname,
    c.relname,
    (c.reltuples)::bigint AS table_num_rows,
    i.relname AS indexrelname,
    pg_stat_get_numscans(i.oid) AS number_of_scans,
    pg_stat_get_tuples_returned(i.oid) AS idx_tup_read,
    pg_stat_get_tuples_fetched(i.oid) AS idx_tup_fetch
   FROM (((pg_class c
     JOIN pg_index x ON ((c.oid = x.indrelid)))
     JOIN pg_class i ON ((i.oid = x.indexrelid)))
     LEFT JOIN pg_namespace n ON ((n.oid = c.relnamespace)))
  WHERE ((c.relkind = ANY (ARRAY['r'::"char", 't'::"char", 'm'::"char"])) AND (n.nspname <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name])) AND (n.nspname !~ '^pg_toast'::text));


--
-- Name: VIEW index_statistics; Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON VIEW index_statistics IS 'Vue de statistiques d''usage et de volume des indexes';


--
-- Name: materialized_view_size; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW materialized_view_size AS
 WITH RECURSIVE pg_inherit(inhrelid, inhparent) AS (
         SELECT pg_inherits.inhrelid,
            pg_inherits.inhparent
           FROM pg_inherits
        UNION
         SELECT child.inhrelid,
            parent.inhparent
           FROM pg_inherit child,
            pg_inherits parent
          WHERE (child.inhparent = parent.inhrelid)
        ), pg_inherit_short AS (
         SELECT pg_inherit.inhrelid,
            pg_inherit.inhparent
           FROM pg_inherit
          WHERE (NOT (pg_inherit.inhparent IN ( SELECT pg_inherit_1.inhrelid
                   FROM pg_inherit pg_inherit_1)))
        )
 SELECT a.table_schema,
    a.table_name,
    a.row_estimate,
    a.tablespace_name,
    a.total_bytes AS total_size,
    a.index_bytes AS index_size,
    a.toast_bytes AS toast_size,
    a.table_bytes AS table_size,
    pg_size_pretty(a.total_bytes) AS total_size_pretty,
    pg_size_pretty(a.index_bytes) AS index_size_pretty,
    pg_size_pretty(a.toast_bytes) AS toast_size_pretty,
    pg_size_pretty(a.table_bytes) AS table_size_pretty
   FROM ( SELECT a_1.oid,
            a_1.table_schema,
            a_1.table_name,
            a_1.tablespace_name,
            a_1.row_estimate,
            a_1.total_bytes,
            a_1.index_bytes,
            a_1.toast_bytes,
            a_1.parent,
            ((a_1.total_bytes - a_1.index_bytes) - COALESCE(a_1.toast_bytes, (0)::numeric)) AS table_bytes
           FROM ( SELECT c.oid,
                    n.nspname AS table_schema,
                    c.relname AS table_name,
                    c.spcname AS tablespace_name,
                    sum(c.reltuples) OVER (PARTITION BY c.parent) AS row_estimate,
                    sum(pg_total_relation_size((c.oid)::regclass)) OVER (PARTITION BY c.parent) AS total_bytes,
                    sum(pg_indexes_size((c.oid)::regclass)) OVER (PARTITION BY c.parent) AS index_bytes,
                    sum(pg_total_relation_size((c.reltoastrelid)::regclass)) OVER (PARTITION BY c.parent) AS toast_bytes,
                    c.parent
                   FROM (( SELECT pg_class.oid,
                            pg_class.reltuples,
                            pg_class.relname,
                            pg_class.relnamespace,
                            pg_class.reltoastrelid,
                            pg_tablespace.spcname,
                            COALESCE(pg_inherit_short.inhparent, pg_class.oid) AS parent
                           FROM ((pg_class
                             LEFT JOIN pg_tablespace ON ((pg_class.reltablespace = pg_tablespace.oid)))
                             LEFT JOIN pg_inherit_short ON ((pg_inherit_short.inhrelid = pg_class.oid)))
                          WHERE (pg_class.relkind = ANY (ARRAY['m'::"char"]))) c
                     LEFT JOIN pg_namespace n ON ((n.oid = c.relnamespace)))) a_1
          WHERE (a_1.oid = a_1.parent)) a
  WHERE ((a.table_schema <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name])) AND (a.table_schema !~ '^pg_toast'::text))
  ORDER BY a.total_bytes DESC;


--
-- Name: schema_size; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW schema_size AS
 WITH detail AS (
         WITH RECURSIVE all_elements AS (
                 SELECT ('base/'::text || l.filename) AS path,
                    x.size,
                    x.access,
                    x.modification,
                    x.change,
                    x.creation,
                    x.isdir
                   FROM pg_ls_dir('base/'::text) l(filename),
                    LATERAL pg_stat_file(('base/'::text || l.filename)) x(size, access, modification, change, creation, isdir)
                UNION ALL
                 SELECT ('pg_tblspc/'::text || l.filename) AS path,
                    x.size,
                    x.access,
                    x.modification,
                    x.change,
                    x.creation,
                    x.isdir
                   FROM pg_ls_dir('pg_tblspc/'::text) l(filename),
                    LATERAL pg_stat_file(('pg_tblspc/'::text || l.filename)) x(size, access, modification, change, creation, isdir)
                UNION ALL
                 SELECT ((u.path || '/'::text) || l.filename),
                    x.size,
                    x.access,
                    x.modification,
                    x.change,
                    x.creation,
                    x.isdir
                   FROM all_elements u,
                    LATERAL pg_ls_dir(u.path) l(filename),
                    LATERAL pg_stat_file(((u.path || '/'::text) || l.filename)) x(size, access, modification, change, creation, isdir)
                  WHERE u.isdir
                ), all_files AS (
                 SELECT all_elements.path,
                    all_elements.size
                   FROM all_elements
                  WHERE (NOT all_elements.isdir)
                ), interesting_files AS (
                 SELECT regexp_replace(regexp_replace(f_1.path, '.*/'::text, ''::text), '\.[0-9]*$'::text, ''::text) AS filename,
                    sum(f_1.size) AS sum
                   FROM pg_database d,
                    all_files f_1
                  WHERE ((d.datname = current_database()) AND (f_1.path ~ (('/'::text || d.oid) || '/[0-9]+(\.[0-9]+)?$'::text)))
                  GROUP BY (regexp_replace(regexp_replace(f_1.path, '.*/'::text, ''::text), '\.[0-9]*$'::text, ''::text))
                )
         SELECT n.nspname AS schema_name,
            sum(f.sum) AS total_schema_size,
            pg_size_pretty(sum(f.sum)) AS total_schema_size_pretty
           FROM (((interesting_files f
             JOIN pg_class c ON (((f.filename)::oid = c.relfilenode)))
             LEFT JOIN pg_class dtc ON (((dtc.reltoastrelid = c.oid) AND (c.relkind = 't'::"char"))))
             JOIN pg_namespace n ON ((COALESCE(dtc.relnamespace, c.relnamespace) = n.oid)))
          GROUP BY n.nspname
          ORDER BY (sum(f.sum)) DESC
        )
 SELECT detail.schema_name,
    detail.total_schema_size,
    detail.total_schema_size_pretty,
    round((detail.total_schema_size / sum(detail.total_schema_size) OVER ()), 2) AS percent_of_total
   FROM detail;


--
-- Name: table_size; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW table_size AS
 WITH RECURSIVE pg_inherit(inhrelid, inhparent) AS (
         SELECT pg_inherits.inhrelid,
            pg_inherits.inhparent
           FROM pg_inherits
        UNION
         SELECT child.inhrelid,
            parent.inhparent
           FROM pg_inherit child,
            pg_inherits parent
          WHERE (child.inhparent = parent.inhrelid)
        ), pg_inherit_short AS (
         SELECT pg_inherit.inhrelid,
            pg_inherit.inhparent
           FROM pg_inherit
          WHERE (NOT (pg_inherit.inhparent IN ( SELECT pg_inherit_1.inhrelid
                   FROM pg_inherit pg_inherit_1)))
        )
 SELECT a.table_schema,
    a.table_name,
    a.row_estimate,
    a.tablespace_name,
    a.total_bytes AS total_size,
    a.index_bytes AS index_size,
    a.toast_bytes AS toast_size,
    a.table_bytes AS table_size,
    pg_size_pretty(a.total_bytes) AS total_size_pretty,
    pg_size_pretty(a.index_bytes) AS index_size_pretty,
    pg_size_pretty(a.toast_bytes) AS toast_size_pretty,
    pg_size_pretty(a.table_bytes) AS table_size_pretty
   FROM ( SELECT a_1.oid,
            a_1.table_schema,
            a_1.table_name,
            a_1.tablespace_name,
            a_1.row_estimate,
            a_1.total_bytes,
            a_1.index_bytes,
            a_1.toast_bytes,
            a_1.parent,
            ((a_1.total_bytes - a_1.index_bytes) - COALESCE(a_1.toast_bytes, (0)::numeric)) AS table_bytes
           FROM ( SELECT c.oid,
                    n.nspname AS table_schema,
                    c.relname AS table_name,
                    c.spcname AS tablespace_name,
                    sum(c.reltuples) OVER (PARTITION BY c.parent) AS row_estimate,
                    sum(pg_total_relation_size((c.oid)::regclass)) OVER (PARTITION BY c.parent) AS total_bytes,
                    sum(pg_indexes_size((c.oid)::regclass)) OVER (PARTITION BY c.parent) AS index_bytes,
                    sum(pg_total_relation_size((c.reltoastrelid)::regclass)) OVER (PARTITION BY c.parent) AS toast_bytes,
                    c.parent
                   FROM (( SELECT pg_class.oid,
                            pg_class.reltuples,
                            pg_class.relname,
                            pg_class.relnamespace,
                            pg_class.reltoastrelid,
                            pg_tablespace.spcname,
                            COALESCE(pg_inherit_short.inhparent, pg_class.oid) AS parent
                           FROM ((pg_class
                             LEFT JOIN pg_tablespace ON ((pg_class.reltablespace = pg_tablespace.oid)))
                             LEFT JOIN pg_inherit_short ON ((pg_inherit_short.inhrelid = pg_class.oid)))
                          WHERE (pg_class.relkind = ANY (ARRAY['r'::"char", 'p'::"char"]))) c
                     LEFT JOIN pg_namespace n ON ((n.oid = c.relnamespace)))) a_1
          WHERE (a_1.oid = a_1.parent)) a
  WHERE ((a.table_schema <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name])) AND (a.table_schema !~ '^pg_toast'::text))
  ORDER BY a.total_bytes DESC;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: table_size_20220506; Type: TABLE; Schema: admin_pg; Owner: -
--

CREATE TABLE table_size_20220506 (
    table_schema name,
    table_name name,
    row_estimate real,
    tablespace_name name,
    total_size numeric,
    index_size numeric,
    toast_size numeric,
    table_size numeric,
    total_size_pretty text,
    index_size_pretty text,
    toast_size_pretty text,
    table_size_pretty text
);


--
-- Name: tables_without_pk; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW tables_without_pk AS
 SELECT tab.table_schema,
    tab.table_name
   FROM (information_schema.tables tab
     LEFT JOIN information_schema.table_constraints tco ON ((((tab.table_schema)::name = (tco.table_schema)::name) AND ((tab.table_name)::name = (tco.table_name)::name) AND ((tco.constraint_type)::text = 'PRIMARY KEY'::text))))
  WHERE (((tab.table_type)::text = 'BASE TABLE'::text) AND ((tab.table_schema)::name <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name])) AND (tco.constraint_name IS NULL))
  ORDER BY tab.table_schema, tab.table_name;


--
-- Name: VIEW tables_without_pk; Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON VIEW tables_without_pk IS 'Tables sans clé primaire';


--
-- Name: tablespace_size; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW tablespace_size AS
 SELECT pg_tablespace.oid,
    pg_tablespace.spcname,
    pg_size_pretty(pg_tablespace_size(pg_tablespace.spcname)) AS pg_size_pretty
   FROM pg_tablespace
  ORDER BY (pg_tablespace_size(pg_tablespace.spcname)) DESC;


--
-- Name: VIEW tablespace_size; Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON VIEW tablespace_size IS ' taille des tablespaces';


--
-- Name: very_long_queries; Type: VIEW; Schema: admin_pg; Owner: -
--

CREATE VIEW very_long_queries AS
 SELECT u.usename,
    to_char((((((pss.total_exec_time)::numeric / (1000)::numeric))::text || ' second'::text))::interval, 'HH24:MI:SS'::text) AS pretty_total_exec_time,
    pss.query,
    pss.calls,
    pss.total_exec_time,
    pss.min_exec_time,
    pss.max_exec_time,
    pss.mean_exec_time,
    pss.stddev_exec_time,
    pss.rows,
    pss.shared_blks_hit
   FROM (public.pg_stat_statements pss
     JOIN pg_user u ON ((u.usesysid = pss.userid)))
  WHERE ((pss.total_exec_time > (350930)::double precision) AND (pss.query !~~ 'FETCH%'::text))
  ORDER BY pss.total_exec_time DESC;


--
-- Name: VIEW very_long_queries; Type: COMMENT; Schema: admin_pg; Owner: -
--

COMMENT ON VIEW very_long_queries IS 'requêtes de plus de 5 minutes';


--
-- PostgreSQL database dump complete
--

