const { Router } = require('express');
const pool = require('../database/connection');
const { validarJWT } = require('../middlewares/validar-jwt'); // El guardia
const { getPrestamos, getSiguienteIdPrestamo, getPrestamosPendientes, getPrestamoDetalles, getPrestamoById, getPrestamosBySocioId, getPrestamosPendientePorSocio, crearPrestamo, updatePrestamo, eliminarPrestamo } = require('../controllers/prestamos.controller');


const router = Router();
// Todas las rutas de abajo requieren que el guardia valide el token
router.use(validarJWT);

router.get('/siguiente-id', getSiguienteIdPrestamo);
router.get('/pendientes', getPrestamosPendientes);
router.get('/detalles/:idprestamos', getPrestamoDetalles);
// Ruta sin parámetro → devuelve todos
router.get('/pendientes/socio', getPrestamosPendientePorSocio);
// Ruta con parámetro → devuelve filtrado por socio
router.get('/pendientes/socio/:idSocio', getPrestamosPendientePorSocio);
router.get('/socio/:idsocio', getPrestamosBySocioId);
// Global: sin idSocio → devuelve todos
router.get('/resumen-socio', async (req, res) => {
    try {
        const prestamosResult = await pool.query(`
      SELECT COUNT(*) AS num_prestamos
      FROM prestamos
      WHERE tipo != 'AYUDA';
    `);

        const pagosResult = await pool.query(`
      SELECT COUNT(*) AS num_pagos
      FROM pagos;
    `);

        res.json({
            num_prestamos: prestamosResult.rows[0].num_prestamos,
            num_pagos: pagosResult.rows[0].num_pagos
        });
    } catch (err) {
        console.error("Error:", err);
        res.status(500).send("Error del servidor");
    }
});

// Por socio: con idSocio → devuelve solo de ese socio
router.get('/resumen-socio/:idSocio', async (req, res) => {
    try {
        const { idSocio } = req.params;

        const prestamosResult = await pool.query(`
      SELECT COUNT(*) AS num_prestamos
      FROM prestamos
      WHERE tipo != 'AYUDA' AND idsocio = $1;
    `, [idSocio]);

        const pagosResult = await pool.query(`
      SELECT COUNT(*) AS num_pagos
      FROM pagos
      WHERE idsocio = $1;
    `, [idSocio]);

        res.json({
            num_prestamos: prestamosResult.rows[0].num_prestamos,
            num_pagos: pagosResult.rows[0].num_pagos
        });
    } catch (err) {
        console.error("Error:", err);
        res.status(500).send("Error del servidor");
    }
});

//Para la lista de Prestamos
router.get('/filtrar', async (req, res) => {
    const { idSocio, fechaInicio, fechaFin } = req.query;

    let query = `
    SELECT * FROM prestamos
    WHERE 1=1
  `;
    let params = [];
    let i = 1;

    if (idSocio) {
        query += ` AND idsocio = $${i++}`;
        params.push(idSocio);
    }
    if (fechaInicio) {
        query += ` AND fechaprestamo::date >= $${i++}::date`;
        params.push(fechaInicio);
    }
    if (fechaFin) {
        query += ` AND fechaprestamo::date <= $${i++}::date`;
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


router.post('/guardar', crearPrestamo);
router.put('/actualizar/:id', updatePrestamo);
router.delete('/eliminar/:id', eliminarPrestamo);

router.get('/:id', getPrestamoById);
router.get('/', getPrestamos);

module.exports = router;