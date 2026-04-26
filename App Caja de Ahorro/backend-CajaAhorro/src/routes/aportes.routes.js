const { Router } = require('express');
const pool = require('../database/connection');
const { validarJWT } = require('../middlewares/validar-jwt'); // El guardia
const { getAportes, getSiguienteIdAporte, getAporteById, crearAporte, updateAporte, eliminarAporte } = require('../controllers/aportes.controller');

const router = Router();

// Todas las rutas de abajo requieren que el guardia valide el token
router.use(validarJWT);

router.get('/', getAportes);
router.get('/siguiente-id', getSiguienteIdAporte);
router.get('/filtrar', async (req, res) => {
    const { idSocio, fechaInicio, fechaFin } = req.query;

    let query = `
    SELECT * FROM aportes
    WHERE 1=1
  `;
    let params = [];
    let i = 1;

    if (idSocio) {
        query += ` AND idsocio = $${i++}`;
        params.push(idSocio);
    }
    if (fechaInicio) {
        query += ` AND fechaaporte::date >= $${i++}::date`;
        params.push(fechaInicio);
    }
    if (fechaFin) {
        query += ` AND fechaaporte::date <= $${i++}::date`;
        params.push(fechaFin);
    }

    try {
        const result = await pool.query(query, params);
        res.json(result.rows);
    } catch (err) {
        console.error("Error en consulta:", err);
        res.status(500).send("Error del servidor");
    }
});
router.post('/guardar', crearAporte);
router.put('/actualizar/:id', updateAporte);
router.delete('/eliminar/:id', eliminarAporte);

router.get('/:id', getAporteById);
module.exports = router;