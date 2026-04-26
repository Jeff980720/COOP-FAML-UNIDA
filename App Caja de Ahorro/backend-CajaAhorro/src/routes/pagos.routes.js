const { Router } = require('express');
const pool = require('../database/connection');
const { validarJWT } = require('../middlewares/validar-jwt'); // El guardia
const {
    getPagos,
    getSiguienteIdPago,
    guardarPago,      // <-- Verifica que en la ruta NO diga 'registrarPagos'
    getPagoById,
    getPagosBySocioId,
    updatePago,       // <-- Verifica que en la ruta NO diga 'actualizarPago'
    eliminarPago
} = require('../controllers/pagos.controller');

const router = Router();


// Todas las rutas de abajo requieren que el guardia valide el token
router.use(validarJWT);

router.get('/siguiente-id', getSiguienteIdPago);
router.get('/socio/:idsocio', getPagosBySocioId);
//Para la lista de Prestamos
router.get('/filtrar', async (req, res) => {
    const { idSocio, fechaInicio, fechaFin } = req.query;

    let query = `
    SELECT * FROM pagos
    WHERE 1=1
  `;
    let params = [];
    let i = 1;

    if (idSocio) {
        query += ` AND idsocio = $${i++}`;
        params.push(idSocio);
    }
    if (fechaInicio) {
        query += ` AND fechaamortizacion::date >= $${i++}::date`;
        params.push(fechaInicio);
    }
    if (fechaFin) {
        query += ` AND fechaamortizacion::date <= $${i++}::date`;
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

router.post('/guardar', guardarPago);
router.put('/actualizar/:id', updatePago);
router.delete('/eliminar/:id', eliminarPago);

router.get('/:id', getPagoById);
router.get('/', getPagos);
module.exports = router;