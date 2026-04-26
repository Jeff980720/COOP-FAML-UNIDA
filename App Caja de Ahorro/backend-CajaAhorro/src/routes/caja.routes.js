const { Router } = require('express');
const { validarJWT } = require('../middlewares/validar-jwt'); // El guardia
const { getTodasLasCajas, getSiguienteIdCaja, getPagoById, crearCaja, updateCaja, eliminarCaja } = require('../controllers/caja.controller');

const router = Router();

// Todas las rutas de abajo requieren que el guardia valide el token
router.use(validarJWT);

router.get('/', getTodasLasCajas);
router.get('/siguiente-id', getSiguienteIdCaja);
router.get('/:id', getPagoById);
router.post('/guardar', crearCaja);
router.put('/actualizar/:id', updateCaja);
router.delete('/eliminar/:id', eliminarCaja);

module.exports = router;