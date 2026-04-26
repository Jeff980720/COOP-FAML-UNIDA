const { Router } = require('express');
const { validarJWT } = require('../middlewares/validar-jwt');
const {
    getAportesReporteERG,
    getAportesTotalesReporteERG,
    getAportesMensualesReporteERM,
    getPrestamosReporteERG,
    getPrestamosMensualesReporteERM,
    getAyudasReporteERG,
    getPrestamosMensualesReporteERG,
    getInteresPrestamosReporteERG,
    getInteresPrestamosMensualesReporteERM,
    getInteresPrestamosMensualesReporteERG,
    getDineroPagadoReporteERG,
    getDineroPagadoMensualEfectivoReporteERM,
    getDineroPagadoMensualReprestamoReporteERM,
    getDineroPagadoMensualReporteERG,
    getInteresPagadoReporteERG,
    getInteresPagadoMensualReporteERM,
    getInteresPagadoMensualReporteERG,
    getIngresosVariosReporteERG,
    getGastosVariosReporteERG,
    getInteresAnuladoReporteERG,
    getAporteIndividualReporteERG,
    getDineroEnCajaReporteERG,
    getCajaAnteriorReporteERM
} = require('../utils/contabilidad');

const router = Router();

// 1. ELIMINAMOS LA SEGUNDA DECLARACIÓN DE validarJWT (Eso causaba el SyntaxError)
// 2. ELIMINAMOS EL REQUIRE DE socios.routes (No debe estar aquí)

// Todas las rutas de reportes requieren Token
router.use(validarJWT);

// 3. CAMBIAMOS "route.get" POR "router.get" (Usamos la variable local 'router')
// También eliminamos las comas al final de cada línea y rutas duplicadas
router.get('/aportes', getAportesReporteERG);
router.get('/aportestotales', getAportesTotalesReporteERG);
router.get('/aportesmensual', getAportesMensualesReporteERM);
router.get('/prestamos', getPrestamosReporteERG);
router.get('/prestamosmensual', getPrestamosMensualesReporteERM);
router.get('/ayudas', getAyudasReporteERG);
router.get('/prestamos/mensual', getPrestamosMensualesReporteERG);
router.get('/interesprestamos', getInteresPrestamosReporteERG);
router.get('/interesprestamosmensual', getInteresPrestamosMensualesReporteERM);
router.get('/interesprestamos/mensual', getInteresPrestamosMensualesReporteERG);
router.get('/dineropagado', getDineroPagadoReporteERG);
router.get('/dineropagadomensualefectivo', getDineroPagadoMensualEfectivoReporteERM);
router.get('/dineropagadomensualreprestamo', getDineroPagadoMensualReprestamoReporteERM);
router.get('/dineropagado/mensual', getDineroPagadoMensualReporteERG);
router.get('/interespagado', getInteresPagadoReporteERG);
router.get('/interespagadomensual', getInteresPagadoMensualReporteERM);
router.get('/interespagado/mensual', getInteresPagadoMensualReporteERG);
router.get('/ingresosvarios', getIngresosVariosReporteERG);
router.get('/gastosvarios', getGastosVariosReporteERG);
router.get('/interesanulado', getInteresAnuladoReporteERG);
router.get('/aporteindividual', getAporteIndividualReporteERG);
router.get('/dinerocaja', getDineroEnCajaReporteERG);
router.get('/caja-anterior', getCajaAnteriorReporteERM);






















module.exports = router;