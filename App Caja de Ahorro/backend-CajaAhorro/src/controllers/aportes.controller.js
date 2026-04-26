const pool = require('../database/connection');
const { actualizarTodoElSocio } = require('../utils/recalculos');

//**********************************SERVICIOS PARA APORTES***************************************

//////////////TRAER TODA LA LISTA DE APORTES //////////////////////////
// app.get('/api/aportes', async (req, res) => {
const getAportes = async (req, res) => {
    try {
        const result = await pool.query('SELECT * FROM aportes ORDER BY fechaaporte DESC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los aportes de la base de datos' });
    }
};

//////////////////////GENERAR EL SIGUIENTE ID PARA NUEVO APORTE////////////////////////
// app.get('/api/aportes/siguiente-id', async (req, res) => {
const getSiguienteIdAporte = async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idaporte) FROM aportes';
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
// app.get('/api/aportes/:id', async (req, res) => {
const getAporteById = async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM aportes WHERE idaporte = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Aporte no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
};

////////////////////////// GUARDAR UN NUEVO APORTE //////////////////////////
// app.post('/api/aportes', async (req, res) => {
const crearAporte = async (req, res) => {
    const { idaporte, fechaaporte, aportado, idsocio, nombresocio, comentario } = req.body;

    try {
        // Nos aseguramos de que sea solo la parte de la fecha (YYYY-MM-DD)
        const fechaParaSQL = typeof fechaaporte === 'string'
            ? fechaaporte.split('T')[0]
            : fechaaporte;

        const query = `
            INSERT INTO aportes (idaporte, fechaaporte, aportado, idsocio, nombresocio, comentario)
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING *`;

        // Asegúrate de que idsocio llegue como string y no como [object Object]
        const values = [idaporte, fechaParaSQL, aportado, idsocio, nombresocio, comentario];
        const result = await pool.query(query, values);

        // 2. RECALCULAR
        await actualizarTodoElSocio(req.body.idsocio); // <--- RECALCULAR

        res.status(201).json(result.rows[0]);
    } catch (err) {
        console.error("Error SQL:", err.detail || err.message);
        res.status(500).json({ error: 'Error al guardar: verifique si el ID ya existe' });
    }
};

////////////////////////// ACTUALIZAR UN APORTE EXISTENTE //////////////////////////
// app.put('/api/aportes/:id', async (req, res) => {
const updateAporte = async (req, res) => {
    const { id } = req.params;
    const { fechaaporte, aportado, idsocio, nombresocio, comentario } = req.body;

    try {
        // Nos aseguramos de que sea solo la parte de la fecha (YYYY-MM-DD)
        const fechaParaSQL = typeof fechaaporte === 'string'
            ? fechaaporte.split('T')[0]
            : fechaaporte;
        const query = `
            UPDATE aportes 
            SET fechaaporte = $1, 
                aportado = $2, 
                idsocio = $3, 
                nombresocio = $4, 
                comentario = $5
            WHERE idaporte = $6
            RETURNING *`;

        // AQUÍ ESTABA EL ERROR: Deben ser 6 valores exactamente.
        const values = [
            fechaParaSQL, // $1
            aportado,     // $2
            idsocio,      // $3
            nombresocio,  // $4
            comentario,   // $5 (Este faltaba en tu arreglo anterior)
            id            // $6
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            // RECALCULAR después de actualizar el monto
            await actualizarTodoElSocio(req.body.idsocio);
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Aporte no encontrado" });
        }
    } catch (err) {
        console.error("Error al actualizar:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
};

/////////////////////////////// ELIMINAR UN APORTE POR ID //////////////////////////
// app.delete('/api/aportes/:id', async (req, res) => {
const eliminarAporte = async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Buscamos el idsocio antes de borrar para no perder la referencia
        const buscarSocio = await pool.query('SELECT idsocio FROM aportes WHERE idaporte = $1', [id]);

        if (buscarSocio.rows.length > 0) {
            const idsocio = buscarSocio.rows[0].idsocio;

            // 2. Borramos el aporte
            await pool.query('DELETE FROM aportes WHERE idaporte = $1', [id]);

            // 3. RECALCULAR el nuevo total (ya sin el registro borrado)
            await actualizarTodoElSocio(idsocio);

            res.json({ message: "Eliminado correctamente y saldo actualizado" });
        } else {
            res.status(404).json({ message: "Aporte no encontrado" });
        }
    } catch (err) {
        console.error("Error al eliminar:", err);
        res.status(500).send("Error al eliminar");
    }
};

module.exports = {
    getAportes,
    getSiguienteIdAporte,
    getAporteById,
    crearAporte,
    updateAporte,
    eliminarAporte
};