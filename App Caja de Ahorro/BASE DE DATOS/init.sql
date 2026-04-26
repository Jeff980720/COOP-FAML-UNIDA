-- 🔹 Eliminar tablas si existen
DROP TABLE IF EXISTS caja CASCADE;
DROP TABLE IF EXISTS aportes CASCADE;
DROP TABLE IF EXISTS pagos CASCADE;
DROP TABLE IF EXISTS prestamos CASCADE;
DROP TABLE IF EXISTS socios CASCADE;
DROP TABLE IF EXISTS usuarios CASCADE;

-- 🔹 Tabla Usuarios
CREATE TABLE usuarios(
    idusuario SERIAL PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    password VARCHAR(255) NOT NULL,
    nombre_completo VARCHAR(100) NOT NULL,
    rol VARCHAR(20) DEFAULT 'admin',
    estado BOOLEAN DEFAULT TRUE
);

-- 🔹 Tabla Socios
CREATE TABLE socios (
    idsocio VARCHAR(50) PRIMARY KEY,
    nombresocio VARCHAR(255) NOT NULL,
    aportado DECIMAL(15, 2) DEFAULT 0,
    montoprestado DECIMAL(15, 2) DEFAULT 0,
    montopagado DECIMAL(15, 2) DEFAULT 0,
    montopendiente DECIMAL(15, 2) DEFAULT 0,
    interesprestado DECIMAL(15, 2) DEFAULT 0,
    interespagado DECIMAL(15, 2) DEFAULT 0,
    interesanulado DECIMAL(15, 2) DEFAULT 0,
    interespendiente DECIMAL(15, 2) DEFAULT 0
);

-- 🔹 Tabla Prestamos
CREATE TABLE prestamos (
    idprestamos INT PRIMARY KEY,
    tipo VARCHAR(50) NOT NULL,
    idsocio VARCHAR(50) NOT NULL,
    nombresocio VARCHAR(50) NOT NULL,
    fechaprestamo DATE NOT NULL,
    montoprestado DECIMAL(15, 2) NOT NULL,
    plazoprestamo INT NOT NULL,
    interesprestamo DECIMAL(15, 2) NOT NULL,
    interesmensual DECIMAL(15, 2),
    interestotal DECIMAL(15, 2),
    amortizacion DECIMAL(15, 2),
    cuota DECIMAL(15, 2),
    total DECIMAL(15, 2),
    comentario TEXT,
    CONSTRAINT fk_socio_prestamo FOREIGN KEY (idsocio) REFERENCES socios(idsocio)
);

-- 🔹 Tabla Pagos
CREATE TABLE pagos (
    idpagos INT PRIMARY KEY,
    tipo VARCHAR(50) NOT NULL,
    idsocio VARCHAR(50) NOT NULL,
    nombresocio VARCHAR(50) NOT NULL,
    idprestamos INT,
    amortizacion DECIMAL(15, 2) NOT NULL DEFAULT 0,
    fechaamortizacion DATE NOT NULL DEFAULT CURRENT_DATE,
    numamortizacion VARCHAR(50) NOT NULL,
    interes DECIMAL(15, 2) NOT NULL DEFAULT 0,
    fechainteres DATE NOT NULL DEFAULT CURRENT_DATE,
    numinteres VARCHAR(50) NOT NULL,
    comentario TEXT NOT NULL,
    CONSTRAINT fk_socio_pago FOREIGN KEY (idsocio) REFERENCES socios(idsocio) ON DELETE CASCADE,
    CONSTRAINT fk_prestamo_pago FOREIGN KEY (idprestamos) REFERENCES prestamos(idprestamos) ON DELETE SET NULL
);

-- 🔹 Tabla Aportes
CREATE TABLE aportes (
    idaporte INT PRIMARY KEY,
    fechaaporte DATE NOT NULL,
    aportado DECIMAL(15, 2) NOT NULL,
    idsocio VARCHAR(50) NOT NULL,
    nombresocio VARCHAR(255) NOT NULL,
    comentario TEXT,
    CONSTRAINT fk_socio_aporte FOREIGN KEY (idsocio) REFERENCES socios(idsocio)
);

-- 🔹 Tabla Caja
CREATE TABLE caja (
    idcaja INT PRIMARY KEY,
    efectivo DECIMAL(15, 2) NOT NULL,
    fechacaja DATE NOT NULL,
    comentario TEXT
);

-- 🔹 Datos iniciales de socios
INSERT INTO socios (idsocio, nombresocio) VALUES
('01', 'SOCIOS INGRESOS'),
('02', 'SOCIOS GASTOS'),
('03', 'TERESA VILCA'),
('04', 'CESAR TOAQUIZA'),
('05', 'ANA TOAQUIZA'),
('06', 'DANNY ALMACHI'),
('07', 'MARLENE TOAQUIZA'),
('08', 'SEGUNDO GUAYTA'),
('09', 'MAYRA TOAQUIZA'),
('10', 'MARCIAL CHILIQUINGA'),
('11', 'LUIS TOAQUIZA'),
('12', 'DIEGO TOAQUIZA'),
('13', 'MARTHA JAQUE'),
('14', 'ROCIO TOAQUIZA'),
('15', 'GUADALUPE TOAQUIZA'),
('16', 'JEFFERSON TOAQUIZA'),
('17', 'DIANA TOAQUIZA'),
('18', 'CRISTIAN CHACHALO');

-- 🔹 Usuario inicial
INSERT INTO usuarios (username, password, nombre_completo, rol, estado)
VALUES ('jeff98', '1998761524', 'Administrador', 'admin', TRUE);

-- 🔹 Poblar tablas desde CSV
COPY aportes(idaporte, fechaaporte, aportado, idsocio, nombresocio, comentario)
FROM '/docker-entrypoint-initdb.d/importar_aportes_postgres.csv'
DELIMITER ';' CSV HEADER;

COPY prestamos(idprestamos, tipo, idsocio, nombresocio, fechaprestamo, montoprestado, plazoprestamo, interesprestamo, interesmensual, interestotal, amortizacion, cuota, total, comentario)
FROM '/docker-entrypoint-initdb.d/importar_prestamos_postgres.csv'
DELIMITER ';' CSV HEADER;

COPY pagos(idpagos, tipo, idsocio, nombresocio, idprestamos, amortizacion, fechaamortizacion, numamortizacion, interes, fechainteres, numinteres, comentario)
FROM '/docker-entrypoint-initdb.d/importar_pagos_postgres.csv'
DELIMITER ';' CSV HEADER;

COPY caja(idcaja, efectivo, fechacaja, comentario)
FROM '/docker-entrypoint-initdb.d/importar_caja_postgres.csv'
DELIMITER ';' CSV HEADER;
