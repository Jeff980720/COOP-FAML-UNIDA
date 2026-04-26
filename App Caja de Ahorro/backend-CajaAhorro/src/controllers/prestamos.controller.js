const pool = require('../database/connection');
const { get } = require('../routes/socios.routes');
const { actualizarTodoElSocio } = require('../utils/recalculos');

//**********************************SERVICIOS PARA PRESTAMOS***************************************

////////////////////// OBTENER TODA LA LISTA DE PRESTAMOS //////////////////////////
// app.get('/api/prestamos', async (req, res) => {
const getPrestamos = async (req, res) => {
    try {
        const query = `
            SELECT 
                p.*,
                -- LÓGICA DE ESTATUS MEJORADA
                CASE 
                    -- Si es Ayuda, marcar siempre como PAGADO
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'PAGADO'

                    -- Si es Préstamo, calcular según el saldo
                    WHEN (
                        p.montoprestado + p.interestotal - 
                        COALESCE((SELECT SUM(amortizacion + interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0)
                    ) <= 0 THEN 'PAGADO'

                    ELSE 'PENDIENTE'
                END AS estatus_dinamico,

                -- LÓGICA DE LETRAS
                CASE 
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'N/A' -- Las ayudas no tienen cuotas
                    ELSE (
                        p.plazoprestamo - 
                        COALESCE((SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos AND pg.numamortizacion != 'EGR'), 0)
                    ) || ' / ' || p.plazoprestamo
                END AS letras_resumen
            FROM prestamos p
            ORDER BY p.fechaprestamo DESC
        `;
        const result = await pool.query(query);
        res.json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: "Error al obtener préstamos" });
    }
};

////////////////////////// GENERAR EL SIGUIENTE ID PARA NUEVO PRESTAMO (PRESTAMO o AYUDA) //////////////////////////
// app.get('/api/prestamos/siguiente-id', async (req, res) => {
const getSiguienteIdPrestamo = async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idprestamos) FROM prestamos';
        const result = await pool.query(query);

        // Si no hay aportes, empezamos en 1. Si hay, sumamos 1 al máximo.
        const ultimoId = result.rows[0].max || 0;
        const siguienteId = ultimoId + 1;

        // Devolvemos el número
        res.json({ siguienteId });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: 'Error al calcular el siguiente ID' });
    }
};


///////////////////////////// OBTENER LA LISTA DE PRESTAMOS PENDIENTES (EXCLUYENDO AYUDA) //////////////////////////
// app.get('/api/prestamos/pendientes', async (req, res) => {
const getPrestamosPendientes = async (req, res) => {
    try {
        const query = `
        SELECT 
            p.idprestamos, 
            s.nombresocio,
            s.idsocio,
            CAST(p.amortizacion AS FLOAT) AS amortizacion_sugerida,
            CAST(p.interesmensual AS FLOAT) AS interes_sugerido,
            (SELECT COUNT(*) FROM pagos WHERE idprestamos = p.idprestamos AND amortizacion > 0) + 1 AS num_amortizacion,
            (SELECT COUNT(*) FROM pagos WHERE idprestamos = p.idprestamos AND interes > 0) + 1 AS num_interes
        FROM prestamos p
        JOIN socios s ON p.idsocio = s.idsocio
        WHERE p.tipo NOT LIKE 'AYUDA%' 
        AND (
            -- Condición 1: Todavía debe capital
            p.montoprestado > (
                SELECT COALESCE(SUM(amortizacion), 0) 
                FROM pagos 
                WHERE idprestamos = p.idprestamos
            )
            OR 
            -- Condición 2: Todavía debe intereses
            p.interestotal > (
                SELECT COALESCE(SUM(interes), 0) 
                FROM pagos 
                WHERE idprestamos = p.idprestamos
            )
        )
        ORDER BY CAST(p.idprestamos AS INTEGER) ASC;
    `;

        const result = await pool.query(query);
        res.json(result.rows || []);
    } catch (err) {
        console.error("Error en pendientes-lista:", err);
        res.status(500).json({ error: 'Error interno al obtener la lista de pendientes' });
    }
};

//////////////////////////////////// OBTENER DETALLES DE UN PRESTAMO PENDIENTE //////////////////////////
// app.get('/api/prestamos/detalles/:idprestamos', async (req, res) => {
const getPrestamoDetalles = async (req, res) => {
    try {
        const { idprestamos } = req.params;
        const query = `
            SELECT 
                p.idprestamos, p.montoprestado, p.fechaprestamo, 
                p.plazoprestamo, p.interestotal, p.comentario, p.interesprestamo,
                (SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos) as letras_pagadas,
                (SELECT SUM(amortizacion) FROM pagos pg WHERE pg.idprestamos = p.idprestamos) as sum_amortizacion,
                (SELECT SUM(interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos) as sum_interes
            FROM prestamos p
            WHERE p.idprestamos = $1`;

        const result = await pool.query(query, [idprestamos]);
        if (result.rows.length === 0) return res.status(404).send("Préstamo no encontrado");

        const p = result.rows[0];

        // --- DATOS BASE ---
        const montoInicial = parseFloat(p.montoprestado);
        const plazoTotal = parseInt(p.plazoprestamo);
        const interesTotalDefinido = parseFloat(p.interestotal);
        const tasaMensual = parseFloat(p.interesprestamo) / 100;
        const tipoAmortizacion = (p.comentario || "").toUpperCase().trim();

        // --- DETECTAR LA LETRA ACTUAL ---
        const letrasPagadas = parseInt(p.letras_pagadas);
        const nSiguiente = letrasPagadas + 1; // La letra que se va a pagar ahora

        let proximaAmortizacion = 0;
        let proximoInteres = 0;

        // --- LÓGICA POR TIPO DE SISTEMA ---

        if (tipoAmortizacion.includes("ALEMANA")) {
            // ALEMÁN: Amortización de capital es SIEMPRE fija
            proximaAmortizacion = montoInicial / plazoTotal;
            // El interés se calcula sobre el saldo de capital que queda
            // Saldo = MontoInicial - (AmortizaciónFija * letras ya pagadas)
            const saldoAlInicioDeEstaLetra = montoInicial - (proximaAmortizacion * letrasPagadas);
            proximoInteres = saldoAlInicioDeEstaLetra * tasaMensual;
            console.log(`(ALEMANA) Letra ${nSiguiente}: Amortización fija de ${proximaAmortizacion.toFixed(2)}, Interés sobre saldo ${saldoAlInicioDeEstaLetra.toFixed(2)} = ${proximoInteres.toFixed(2)}`);
        }
        else if (tipoAmortizacion.includes("FRANCESA")) {
            // FRANCÉS: Cuota total es fija, pero el desglose cambia cada mes
            const cuotaFija = (montoInicial + interesTotalDefinido) / plazoTotal;

            // Simulamos la tabla hasta llegar a la letra nSiguiente
            let saldoTemporal = montoInicial;
            for (let i = 1; i <= nSiguiente; i++) {
                proximoInteres = saldoTemporal * tasaMensual;
                proximaAmortizacion = cuotaFija - proximoInteres;
                saldoTemporal -= proximaAmortizacion;
            }
            console.log(`(FRANCESA) Letra ${nSiguiente}: Cuota fija de ${cuotaFija.toFixed(2)}, Amortización ${proximaAmortizacion.toFixed(2)}, Interés ${proximoInteres.toFixed(2)}, Saldo restante ${saldoTemporal.toFixed(2)}`);
        }
        else {
            // FAMILIAR / LINEAL / DEFAULT: Todo se divide para el plazo
            proximaAmortizacion = montoInicial / plazoTotal;
            proximoInteres = interesTotalDefinido / plazoTotal;
            console.log(`(FAMILIAR/LINEAL) Letra ${nSiguiente}: Amortización ${proximaAmortizacion.toFixed(2)}, Interés ${proximoInteres.toFixed(2)}`);
        }

        // --- PROTECCIÓN FIN DE PLAZO ---
        if (nSiguiente > plazoTotal) {
            proximaAmortizacion = 0;
            proximoInteres = 0;
        }

        // --- RESPUESTA PARA EL FRONTEND ---
        res.json({
            montoPrestado: montoInicial,
            fechaPrestamo: p.fechaprestamo,
            plazoPrestamo: plazoTotal,
            tipoAmortizacion: tipoAmortizacion,
            // Valores que cargan los inputs de Amortización e Interés
            proximaAmortizacion: parseFloat(proximaAmortizacion.toFixed(2)),
            proximoInteres: parseFloat(proximoInteres.toFixed(2)),
            cuotaPrestamo: parseFloat((proximaAmortizacion + proximoInteres).toFixed(2)),
            // Información para etiquetas y contadores
            numLetra: nSiguiente,
            letrasPagadas: letrasPagadas,
            letrasPendientes: Math.max(0, plazoTotal - letrasPagadas),
            amortizacionPendiente: (montoInicial - (parseFloat(p.sum_amortizacion) || 0)).toFixed(2),
            interesPendiente: (interesTotalDefinido - (parseFloat(p.sum_interes) || 0)).toFixed(2)
        });

    } catch (err) {
        console.error("Error:", err.message);
        res.status(500).send("Error interno al calcular detalles");
    }
};

/////////////////////////// OBTENER UN PRESTAMO PENDIENTE POR SOCIO //////////////////////////
// const getPrestamosPendientePorSocio = async (req, res) => {
//     try {
//         const query = `
//             SELECT * FROM (
//                 SELECT 
//                     s.idsocio,
//                     (s.nombresocio) as nombre_socio,
//                     p.idprestamos, 
//                     p.fechaprestamo,
//                     p.montoprestado as capital_inicial,
//                     p.interestotal as interes_inicial,
//                     -- Cálculo de Capital Pendiente
//                     (p.montoprestado - (SELECT COALESCE(SUM(amortizacion),0) FROM pagos pg WHERE pg.idprestamos = p.idprestamos)) as capital_pendiente,
//                     -- Cálculo de Interés Pendiente
//                     (p.interestotal - (SELECT COALESCE(SUM(interes),0) FROM pagos pg WHERE pg.idprestamos = p.idprestamos)) as interes_pendiente,
//                     -- Cálculo de Letras/Cuotas Pendientes
//                     (p.plazoprestamo - (SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos)) as letras_pendientes
//                 FROM prestamos p
//                 INNER JOIN socios s ON p.idsocio = s.idsocio
//             ) as reporte
//             WHERE (capital_pendiente + interes_pendiente) > 0
//             ORDER BY idprestamos ASC`;

//         const result = await pool.query(query);

//         // Mapeo para el frontend (Dashboard)
//         const resumenDetallado = result.rows.map(row => ({
//             idSocio: row.idsocio,
//             socio: row.nombre_socio,
//             idPrestamo: row.idprestamos,
//             fecha: row.fechaprestamo,
//             capitalInicial: parseFloat(row.capital_inicial),
//             interesInicial: parseFloat(row.interes_inicial),
//             capitalPendiente: parseFloat(row.capital_pendiente),
//             interesPendiente: parseFloat(row.interes_pendiente),
//             letrasPendientes: parseInt(row.letras_pendientes),
//             totalPendiente: parseFloat(row.capital_pendiente) + parseFloat(row.interes_pendiente)
//         }));

//         res.json(resumenDetallado);
//     } catch (err) {
//         console.error("Error en reporte de préstamos:", err);
//         res.status(500).send("Error al generar la lista de préstamos pendientes");
//     }
// };

const getPrestamosPendientePorSocio = async (req, res) => {
    try {
        // Limpiamos el idSocio: si es "null", "undefined" o vacío, lo tratamos como null real
        let { idSocio } = req.params;
        if (idSocio === 'undefined' || idSocio === 'null' || idSocio === '') {
            idSocio = null;
        }

        const query = `
            SELECT * FROM (
                SELECT 
                    s.idsocio,
                    s.nombresocio as nombre_socio,
                    p.idprestamos, 
                    p.fechaprestamo,
                    p.tipo as tipoprestamo, -- Asegúrate que este sea el nombre real en tu DB
                    p.montoprestado as capital_inicial,
                    p.interestotal as interes_inicial,
                    (p.montoprestado - (SELECT COALESCE(SUM(amortizacion),0) FROM pagos pg WHERE pg.idprestamos = p.idprestamos)) as capital_pendiente,
                    (p.interestotal - (SELECT COALESCE(SUM(interes),0) FROM pagos pg WHERE pg.idprestamos = p.idprestamos)) as interes_pendiente,
                    (p.plazoprestamo - (SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos)) as letras_pendientes
                FROM prestamos p
                INNER JOIN socios s ON p.idsocio = s.idsocio
                WHERE p.tipo != 'AYUDA'
               ${idSocio ? 'AND p.idsocio = $1' : ''}
            ) as reporte
            WHERE (capital_pendiente + interes_pendiente) > 0
            ORDER BY idprestamos ASC`;

        const result = idSocio
            ? await pool.query(query, [idSocio])
            : await pool.query(query);

        const resumenDetallado = result.rows.map(row => ({
            idSocio: row.idsocio,
            socio: row.nombre_socio,
            idPrestamo: row.idprestamos,
            tipo: row.tipoprestamo,
            fecha: row.fechaprestamo,
            capitalPendiente: parseFloat(row.capital_pendiente),
            interesPendiente: parseFloat(row.interes_pendiente),
            letrasPendientes: parseInt(row.letras_pendientes),
            totalPendiente: parseFloat(row.capital_pendiente) + parseFloat(row.interes_pendiente)
        }));

        res.json(resumenDetallado);
    } catch (err) {
        console.error("Error:", err);
        res.status(500).send("Error del servidor");
    }
};

/////////////////////////// OBTENER UN PRESTAMO POR ID //////////////////////////
// app.get('/api/prestamos/:id', async (req, res) => {
const getPrestamoById = async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM prestamos WHERE idprestamos = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Préstamo no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
};

/////////////////////////// OBTENER LOS PRESTAMOS POR ID SOCIO //////////////////////////
// app.get('/api/prestamos/socio/:idsocio', async (req, res) => {
const getPrestamosBySocioId = async (req, res) => {
    const { idsocio } = req.params;
    try {
        const result = await pool.query(
            `SELECT 
                p.*,
                -- LÓGICA DE ESTATUS MEJORADA
                CASE 
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'PAGADO'
                    WHEN (
                        p.montoprestado + p.interestotal - 
                        COALESCE((SELECT SUM(amortizacion + interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0)
                    ) <= 0 THEN 'PAGADO'
                    ELSE 'PENDIENTE'
                END AS estatus_dinamico,

                -- LÓGICA DE LETRAS
                CASE 
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'N/A'
                    ELSE (
                        p.plazoprestamo - 
                        COALESCE((SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos AND pg.numamortizacion != 'EGR'), 0)
                    ) || ' / ' || p.plazoprestamo
                END AS letras_resumen
            FROM prestamos p
            WHERE p.idsocio = $1
            ORDER BY p.fechaprestamo ASC`,
            [idsocio]
        );
        console.log('lista de prestamos por socio', result.rows);
        res.json(result.rows);

    } catch (err) {
        console.error("Error detalle:", err.message);
        res.status(500).send("Error en el servidor al obtener préstamos del socio");
    }
};

///////////////////////////// GUARDAR UN NUEVO PRESTAMO //////////////////////////
// app.post('/api/prestamos', async (req, res) => {
const crearPrestamo = async (req, res) => {
    // 1. Agregamos 'tipo' a la desestructuración del body
    const {
        idprestamos, tipo, idsocio, nombresocio, fechaprestamo,
        montoprestado, plazoprestamo, interesprestamo, interesmensual,
        interestotal, amortizacion, cuota, total, comentario
    } = req.body;

    try {
        // 2. Agregamos 'tipo' a la consulta SQL y un nuevo marcador $14
        const query = `
            INSERT INTO prestamos (
                idprestamos, tipo, idsocio, nombresocio, fechaprestamo, 
                montoprestado, plazoprestamo, interesprestamo, interesmensual, 
                interestotal, amortizacion, cuota, total, comentario
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
            RETURNING *`;

        // 3. Incluimos 'tipo' en el array de valores (asegúrate que el orden coincida con el INSERT)
        const values = [
            idprestamos,
            tipo || 'PRESTAMO', // Salvaguarda por si llega vacío
            idsocio,
            nombresocio,
            fechaprestamo,
            montoprestado,
            plazoprestamo,
            interesprestamo,
            interesmensual,
            interestotal,
            amortizacion,
            cuota,
            total,
            comentario
        ];

        const result = await pool.query(query, values);

        // Recalcular datos del socio (Muy bien que lo tengas así)
        await actualizarTodoElSocio(idsocio);

        res.status(201).json(result.rows[0]);
    } catch (err) {
        console.error("Error en POST /api/prestamos:", err.detail || err.message);
        res.status(500).json({
            error: 'Error al insertar el registro',
            detalle: err.detail
        });
    }
};

//////////////////////////////// ACTUALIZAR UN PRESTAMO EXISTENTE //////////////////////////
// app.put('/api/prestamos/:id', async (req, res) => {
const updatePrestamo = async (req, res) => {
    const { id } = req.params;
    const {
        idsocio, nombresocio, fechaprestamo, montoprestado, plazoprestamo,
        interesprestamo, interesmensual, interestotal, amortizacion,
        cuota, total, comentario, tipo // Asegúrate de recibir 'tipo' para la validación
    } = req.body;

    try {
        // --- 1. VALIDACIÓN DE ESTATUS DINÁMICO ANTES DE ACTUALIZAR ---
        const checkStatusQuery = `
            SELECT 
                p.tipo,
                (
                    p.montoprestado + p.interestotal - 
                    COALESCE((SELECT SUM(amortizacion + interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0)
                ) AS saldo_pendiente
            FROM prestamos p 
            WHERE p.idprestamos = $1`;

        const checkResult = await pool.query(checkStatusQuery, [id]);

        if (checkResult.rows.length === 0) {
            return res.status(404).json({ message: "Préstamo no encontrado" });
        }

        const prestamoActual = checkResult.rows[0];
        const esAyuda = prestamoActual.tipo && prestamoActual.tipo.startsWith('AYUDA');
        const estaPagado = parseFloat(prestamoActual.saldo_pendiente) <= 0;

        // Bloqueamos si es AYUDA o si el saldo es 0 (PAGADO)
        if (esAyuda || estaPagado) {
            return res.status(400).json({
                error: "BLOQUEADO",
                message: "No se puede editar un préstamo con estatus PAGADO o tipo AYUDA."
            });
        }

        // --- 2. EJECUCIÓN DEL UPDATE (Si pasa la validación) ---
        const query = `
            UPDATE prestamos 
            SET idsocio = $1, nombresocio = $2, fechaprestamo = $3, 
                montoprestado = $4, plazoprestamo = $5, interesprestamo = $6, 
                interesmensual = $7, interestotal = $8, amortizacion = $9, 
                cuota = $10, total = $11, comentario = $12
            WHERE idprestamos = $13
            RETURNING *`;

        const values = [
            idsocio, nombresocio, fechaprestamo, montoprestado, plazoprestamo,
            interesprestamo, interesmensual, interestotal, amortizacion,
            cuota, total, comentario, id
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            // Recalculamos totales del socio (tu función existente)
            await actualizarTodoElSocio(idsocio);
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Error al actualizar: Registro no encontrado" });
        }

    } catch (err) {
        console.error("Error al actualizar préstamo:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
};

/////////////////////////////// ELIMINAR UN PRESTAMO POR ID //////////////////////////
// app.delete('/api/prestamos/:id', async (req, res) => {
const eliminarPrestamo = async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Verificar si el préstamo tiene pagos
        const consultaPagos = await pool.query('SELECT * FROM pagos WHERE idprestamos = $1', [id]);

        if (consultaPagos.rows.length > 0) {
            return res.status(400).json({
                error: 'Restricción de Integridad',
                message: `No se puede eliminar el préstamo #${id} porque ya tiene pagos registrados.`
            });
        }

        // 2. OBTENER EL IDSOCIO antes de borrar (Esto lo tienes bien)
        const datosPrestamo = await pool.query('SELECT idsocio FROM prestamos WHERE idprestamos = $1', [id]);

        if (datosPrestamo.rows.length === 0) {
            return res.status(404).json({ message: 'Préstamo no encontrado' });
        }

        const idsocioFinal = datosPrestamo.rows[0].idsocio;

        // 3. Proceder a borrar
        await pool.query('DELETE FROM prestamos WHERE idprestamos = $1', [id]);

        // 4. USAR LA VARIABLE idsocioFinal (NO req.body)
        await actualizarTodoElSocio(idsocioFinal);

        res.json({ message: 'Registro eliminado correctamente y saldo de préstamos actualizado' });

    } catch (err) {
        console.error("Error al eliminar préstamo:", err);
        res.status(500).json({ error: 'Error interno del servidor' });
    }
};

module.exports = {
    getPrestamos,
    getSiguienteIdPrestamo,
    getPrestamosPendientes,
    getPrestamoDetalles,
    getPrestamosPendientePorSocio,
    getPrestamoById,
    getPrestamosBySocioId,
    crearPrestamo,
    updatePrestamo,
    eliminarPrestamo
};