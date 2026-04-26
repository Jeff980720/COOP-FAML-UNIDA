const pool = require('../database/connection');
const { actualizarTodoElSocio } = require('../utils/recalculos');

//**********************************SERVICIOS PARA CAJA***************************************

//////////////TRAER TODA LA LISTA DE CAJA //////////////////////////
// app.get('/api/caja', async (req, res) => {
const getTodasLasCajas = async (req, res) => {
    try {
        const result = await pool.query('SELECT * FROM caja ORDER BY fechacaja DESC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los registros de caja' });
    }
};

//////////////////////GENERAR EL SIGUIENTE ID PARA NUEVO APORTE////////////////////////
// app.get('/api/caja/siguiente-id', async (req, res) => {
const getSiguienteIdCaja = async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idcaja) FROM caja';
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

///////////////////////TRAER UN APORTE POR ID //////////////////////////
// app.get('/api/caja/:id', async (req, res) => {
const getPagoById = async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM caja WHERE idcaja = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Registro de caja no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
};

////////////////////////// GUARDAR NUEVA CAJA //////////////////////////
// app.post('/api/caja', async (req, res) => {
const crearCaja = async (req, res) => {
    const { idcaja, efectivo, fechacaja, comentario } = req.body;

    // 1. Calcular el rango del período basado en fechacaja
    const fechaActual = new Date(fechacaja);
    let inicioPeriodo = new Date(fechaActual);
    let finPeriodo = new Date(fechaActual);

    if (fechaActual.getDate() >= 15) {
        // Ejemplo: 16 de Enero -> Rango: 15/Ene al 14/Feb
        inicioPeriodo.setDate(15);
        finPeriodo.setMonth(finPeriodo.getMonth() + 1);
        finPeriodo.setDate(14);
    } else {
        // Ejemplo: 10 de Enero -> Rango: 15/Dic al 14/Ene
        inicioPeriodo.setMonth(inicioPeriodo.getMonth() - 1);
        inicioPeriodo.setDate(15);
        finPeriodo.setDate(14);
    }

    // Normalizar horas para evitar errores de comparación
    inicioPeriodo.setHours(0, 0, 0, 0);
    finPeriodo.setHours(23, 59, 59, 999);

    try {
        // 2. Verificar si ya existe un registro en ese rango
        const checkQuery = `
            SELECT idcaja FROM caja 
            WHERE fechacaja BETWEEN $1 AND $2 
            LIMIT 1`;

        const checkResult = await pool.query(checkQuery, [inicioPeriodo, finPeriodo]);

        if (checkResult.rows.length > 0) {
            return res.status(400).json({
                error: `Ya existe un registro para el periodo ${inicioPeriodo.toLocaleDateString()} al ${finPeriodo.toLocaleDateString()}`
            });
        }

        // 3. Si no existe, procedemos al INSERT
        const insertQuery = `
            INSERT INTO caja (idcaja, efectivo, fechacaja, comentario)
            VALUES ($1, $2, $3, $4)
            RETURNING *`;

        const values = [idcaja, efectivo, fechacaja, comentario];
        const result = await pool.query(insertQuery, values);

        res.status(201).json(result.rows[0]);

    } catch (err) {
        console.error("Error SQL:", err.detail || err.message);
        res.status(500).json({ error: 'Error interno del servidor' });
    }
};

////////////////////////// ACTUALIZAR CAJA EXISTENTE //////////////////////////
// app.put('/api/caja/:id', async (req, res) => {
const updateCaja = async (req, res) => {
    const { id } = req.params;
    const { efectivo, fechacaja, comentario } = req.body;

    try {
        const query = `
            UPDATE caja 
            SET efectivo = $1, 
                fechacaja = $2, 
                comentario = $3
            WHERE idcaja = $4
            RETURNING *`;

        // AQUÍ ESTABA EL ERROR: Deben ser 6 valores exactamente.
        const values = [
            efectivo, // $1
            fechacaja,     // $2
            comentario,   // $3
            id            // $4
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Caja no encontrada" });
        }
    } catch (err) {
        console.error("Error al actualizar:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
};

/////////////////////////////// ELIMINAR UN CAJA POR ID //////////////////////////
// app.delete('/api/caja/:id', async (req, res) => {
const eliminarCaja = async (req, res) => {
    const { id } = req.params;
    try {
        await pool.query('DELETE FROM caja WHERE idcaja = $1', [id]);
        res.json({ message: "Eliminado correctamente" });
    } catch (err) {
        res.status(500).send("Error al eliminar");
    }
};

module.exports = {
    getTodasLasCajas,
    getSiguienteIdCaja,
    getPagoById,
    crearCaja,
    updateCaja,
    eliminarCaja
};