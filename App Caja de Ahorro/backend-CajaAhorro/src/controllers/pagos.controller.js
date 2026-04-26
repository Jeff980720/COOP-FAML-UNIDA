const pool = require('../database/connection');
const { actualizarTodoElSocio } = require('../utils/recalculos');

//**********************************SERVICIOS PARA PAGOS***************************************

//////////////TRAER TODA LA LISTA DE PAGOS //////////////////////////
// app.get('/api/pagos', async (req, res) => {
const getPagos = async (req, res) => {
    try {
        const result = await pool.query('SELECT * FROM pagos  ORDER BY fechaamortizacion DESC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los pagos de la base de datos' });
    }
};

///////////////////////////////// GENERAR EL SIGUIENTE ID PARA PAGOS (PAGO, INGRESO, EGRESO) //////////////////////////
// app.get('/api/pagos/siguiente-id', async (req, res) => {
const getSiguienteIdPago = async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idpagos) FROM pagos';
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

///////////////////////////// GUARDAR UN NUEVO PAGO //////////////////////////
// app.post('/api/pagos/guardar', async (req, res) => {
const guardarPago = async (req, res) => {
    const {
        idpagos, tipo, idsocio, nombresocio, idprestamos,
        amortizacion, fechaamortizacion, numamortizacion,
        interes, fechainteres, numinteres, comentario
    } = req.body;

    // 1. Manejo de idprestamos para evitar errores de FK
    const idPrestamosFinal = (idprestamos === '0' || idprestamos === 0 || idprestamos === '' || !idprestamos)
        ? null
        : idprestamos;

    // 2. Asegurar que los montos no sean undefined/null para evitar errores en la BD
    const amortizacionFinal = amortizacion || 0;
    const interesFinal = interes || 0;

    try {
        const query = `
            INSERT INTO pagos (
                idpagos, tipo, idsocio, nombresocio, idprestamos, 
                amortizacion, fechaamortizacion, numamortizacion, 
                interes, fechainteres, numinteres, comentario
            ) 
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
            RETURNING *`;

        const values = [
            idpagos, tipo, idsocio, nombresocio, idPrestamosFinal,
            amortizacionFinal, fechaamortizacion, numamortizacion,
            interesFinal, fechainteres, numinteres, comentario
        ];

        const result = await pool.query(query, values);
        await actualizarTodoElSocio(req.body.idsocio); // <--- RECALCULAR
        res.status(201).json(result.rows[0]);

    } catch (err) {
        console.error('Error en el servidor:', err);
        // Enviamos el mensaje de error real para debuguear más rápido
        res.status(500).json({ error: 'Error al insertar el registro', detail: err.message });
    }
};

/////////////////////////// OBTENER UN PAGO POR ID //////////////////////////
// app.get('/api/pagos/:id', async (req, res) => {
const getPagoById = async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM pagos WHERE idpagos = $1 ORDER BY fechaamortizacion ASC', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Pago no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
};

/////////////////////////// OBTENER LOS PAGOS POR ID SOCIO //////////////////////////
// app.get('/api/pagos/socio/:idsocio', async (req, res) => {
const getPagosBySocioId = async (req, res) => {
    const { idsocio } = req.params;
    try {
        const query = `
            -- 1. CONSULTA DE PAGOS REALES
            SELECT 
                pg.idpagos, pg.idprestamos, pg.idsocio, pg.amortizacion, 
                pg.fechaamortizacion, pg.numamortizacion, pg.interes, 
                pg.fechainteres, pg.numinteres, pg.comentario, 
                pr.tipo,
                -- ASIGNACIÓN MANUAL DEL NOMBRE SEGÚN EL ID
                CASE 
                    WHEN $1 = '01' THEN 'SOCIOS INGRESOS'
                    WHEN $1 = '02' THEN 'SOCIOS GASTOS'
                    ELSE s.nombresocio 
                END AS nombresocio
            FROM pagos pg
            INNER JOIN socios s ON pg.idsocio = s.idsocio
            LEFT JOIN prestamos pr ON pg.idprestamos = pr.idprestamos
            WHERE 
                (pg.idsocio = $1 AND $1 NOT IN ('01', '02'))
                OR
                ($1 = '01' AND pg.numamortizacion = 'IGR')
                OR
                ($1 = '02' AND pg.idsocio = '02')

            UNION ALL

            -- 2. CONSULTA DE AYUDAS (SOLO PARA SOCIO 02)
            SELECT 
                NULL AS idpagos, pr.idprestamos, pr.idsocio, 
                pr.montoprestado AS amortizacion, pr.fechaprestamo AS fechaamortizacion, 
                'AYUDA' AS numamortizacion, 0 AS interes, NULL AS fechainteres, 
                'AYUDA' AS numinteres, 'REGISTRO DE AYUDA SOCIAL' AS comentario, 
                pr.tipo,
                'SOCIOS GASTOS' AS nombresocio -- ASIGNACIÓN MANUAL DIRECTA
            FROM prestamos pr
            WHERE $1 = '02' 
              AND pr.tipo LIKE 'AYUDA%' 
              AND pr.montoprestado > 0

            ORDER BY fechaamortizacion ASC
        `;

        const result = await pool.query(query, [idsocio]);
        res.json(result.rows);

    } catch (err) {
        console.error("Error detalle:", err.message);
        res.status(500).json({ error: "Error al procesar la consulta" });
    }
};

//////////////////////////////// ACTUALIZAR UN PAGO EXISTENTE //////////////////////////
// app.put('/api/pagos/:id', async (req, res) => {
const updatePago = async (req, res) => {
    const { id } = req.params;
    const { idsocio, nombresocio, idprestamos, amortizacion, fechaamortizacion, numamortizacion, interes, fechainteres, numinteres, comentario } = req.body;

    try {
        const query = `
            UPDATE pagos 
            SET idsocio = $1, nombresocio = $2, idprestamos = $3, amortizacion = $4, 
                fechaamortizacion = $5, numamortizacion = $6, interes = $7, 
                fechainteres = $8, numinteres = $9, comentario = $10
            WHERE idpagos = $11 RETURNING *`;

        const values = [idsocio, nombresocio, idprestamos, amortizacion, fechaamortizacion, numamortizacion, interes, fechainteres, numinteres, comentario, id];
        const result = await pool.query(query, values);
        await actualizarTodoElSocio(req.body.idsocio);
        res.json(result.rows[0]);
    } catch (err) {
        console.error(err);
        res.status(500).send('Error del servidor');
    }
};

///////////////////////////// ELIMINAR UN PAGO POR ID //////////////////////////
// app.delete('/api/pagos/:id', async (req, res) => {
const eliminarPago = async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Buscamos el idsocio antes de borrar el registro
        const resPago = await pool.query('SELECT idsocio FROM pagos WHERE idpagos = $1', [id]);

        if (resPago.rows.length === 0) {
            return res.status(404).json({ error: "Pago no encontrado" });
        }

        const idsocio = resPago.rows[0].idsocio;

        // 2. Eliminamos el pago
        await pool.query('DELETE FROM pagos WHERE idpagos = $1', [id]);

        // 3. Ahora sí, recalculamos con el idsocio que guardamos
        await actualizarTodoElSocio(idsocio);

        res.json({ message: "Pago eliminado y saldos del socio actualizados" });
    } catch (err) {
        console.error(err.message);
        res.status(500).send("Error al eliminar el pago");
    }
};

module.exports = {
    getPagos,
    getSiguienteIdPago,
    guardarPago,      // <-- Verifica que en la ruta NO diga 'registrarPagos'
    getPagoById,
    getPagosBySocioId,
    updatePago,       // <-- Verifica que en la ruta NO diga 'actualizarPago'
    eliminarPago
};