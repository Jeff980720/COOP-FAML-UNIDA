const { Router } = require('express');
const { validarJWT } = require('../middlewares/validar-jwt'); // El guardia
const { getSocios, getSociosReporte, getSiguienteIdSocio, getSocioById, crearSocio, updateSocio, eliminarSocio } = require('../controllers/socios.controller');

const router = Router();

// Todas las rutas de abajo requieren que el guardia valide el token
router.use(validarJWT);

router.get('/', getSocios);
router.get('/reporte', getSociosReporte);
router.get('/siguiente-id', getSiguienteIdSocio);
router.get('/:id', getSocioById);
router.post('/guardar', crearSocio);
router.put('/actualizar/:id', updateSocio);
router.delete('/eliminar/:id', eliminarSocio);

module.exports = router;