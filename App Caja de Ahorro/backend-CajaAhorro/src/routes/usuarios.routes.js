const { Router } = require('express');
const { getUsuarios, crearUsuario, actualizarUsuario, eliminarUsuario } = require('../controllers/usuarios.controller');
const { validarJWT } = require('../middlewares/validar-jwt');

const router = Router();

// Aplicar el middleware de protección que ya tienes funcionando
router.use(validarJWT);

router.get('/', getUsuarios);
router.post('/', crearUsuario);
// SOLUCIÓN: Usar parámetros dinámicos con ":" y sin el punto "." inicial
router.put('/:id', actualizarUsuario);
router.delete('/:id', eliminarUsuario);

module.exports = router;