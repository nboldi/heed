
CREATE TABLE nodes ( 
	type VARCHAR(50), 
	ctor VARCHAR(50), 
	file VARCHAR(200), 
	start_row INT, 
	start_col INT, 
	end_row INT,
	end_col INT,
	parent_type INT,
	parent_start_row INT, 
	parent_start_col INT, 
	parent_end_row INT,
	parent_end_col INT,
	parent_handle VARCHAR(50),
	PRIMARY KEY( file, start_row, start_col, end_row, end_col, type )
);

CREATE TABLE name_infos ( 
	file VARCHAR(200),
	start_row INT, 
	start_col INT, 
	end_row INT,
	end_col INT,
	def_file VARCHAR(200),
	def_start_row INT, 
	def_start_col INT, 
	def_end_row INT,
	def_end_col INT,
	namespace VARCHAR(20),
	name VARCHAR(200),
	uniq VARCHAR(20),
	PRIMARY KEY( file, start_row, start_col, end_row, end_col )
);

CREATE TABLE scopes (
	scope_id INTEGER PRIMARY KEY,
	file VARCHAR(200),
	start_row INT,
	start_col INT,
	end_row INT,
	end_col INT
);

CREATE TABLE scope_names (
	scope_id INTEGER,
	name VARCHAR(200),
	namespace VARCHAR(20),
	uniq VARCHAR(20)
);

CREATE TABLE tokens (
	file VARCHAR(200), 
	start_row INT, 
	start_col INT, 
	end_row INT,
	end_col INT,
	token VARCHAR(50),
	PRIMARY KEY( file, start_row, start_col )
);

CREATE TABLE comments (
	file VARCHAR(200), 
	start_row INT, 
	start_col INT, 
	end_row INT,
	end_col INT,
	type VARCHAR(20),
	content TEXT,
	PRIMARY KEY( file, start_row, start_col )
);

