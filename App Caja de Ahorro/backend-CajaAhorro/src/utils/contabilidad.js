const pool = require('../database/connection');

//////////////////////////PETICIONES PARA INFORMES /////////////////////
// PETICIONES PARA INFORMES //
// app.get('/api/reportes/aportes', async (req, res) => {
const getAportesReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(aportado), 0) as total 
            FROM aportes 
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroAportado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte aportes:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/aportestotales', async (req, res) => {
const getAportesTotalesReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(aportado), 0) as total 
            FROM aportes 
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            totalAportados: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte aportes:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/aportesmensual', async (req, res) => {
const getAportesMensualesReporteERM = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(aportado), 0) as total 
            FROM aportes 
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroAportado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte aportes:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/prestamos', async (req, res) => {
const getPrestamosReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroEnPrestamo: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/prestamosmensual', async (req, res) => {
const getPrestamosMensualesReporteERM = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPrestado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/ayudas', async (req, res) => {
const getAyudasReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroAyuda: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/prestamos/mensual', async (req, res) => {
const getPrestamosMensualesReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPrestadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/interesprestamos', async (req, res) => {
const getInteresPrestamosReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interestotal), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesEnPrestamo: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes prestamo:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/interesprestamosmensual', async (req, res) => {
const getInteresPrestamosMensualesReporteERM = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interestotal), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPrestado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes prestamo:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/interesprestamos/mensual', async (req, res) => {
const getInteresPrestamosMensualesReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interestotal), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPrestadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes prestamo mensual:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/dineropagado', async (req, res) => {
const getDineroPagadoReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo NOT LIKE 'INGRESO%' AND idsocio!= '02'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPaga: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/dineropagadomensualefectivo', async (req, res) => {
const getDineroPagadoMensualEfectivoReporteERM = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo NOT LIKE 'INGRESO%' AND idsocio!= '02' AND comentario !~* 'RE+PRESTAMO'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPagadoEfectivo: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/dineropagadomensualreprestamo', async (req, res) => {
const getDineroPagadoMensualReprestamoReporteERM = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(
                SUM(
                    CASE 
                        WHEN tipo NOT LIKE 'INGRESO%' AND idsocio != '02' AND (comentario LIKE '%REPRESTAMO%' OR comentario LIKE '%REEPRESTAMO%')
                        THEN (COALESCE(amortizacion, 0))

                        WHEN tipo LIKE 'EGRESO%' AND idsocio= '02' 
                        THEN -(COALESCE(amortizacion, 0) + COALESCE(interes, 0)) -- El signo (-) resta este grupo

                        ELSE 0 
                    END
                ), 0) as total_neto
            FROM pagos
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date;
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPagadoReprestamo: parseFloat(result.rows[0].total_neto)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/dineropagado/mensual', async (req, res) => {
const getDineroPagadoMensualReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo NOT LIKE 'EGRESO%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPagadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado mensual:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/interespagado', async (req, res) => {
const getInteresPagadoReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interes), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo NOT LIKE 'INGRESO%' AND idsocio!= '02'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPagado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/interespagadomensual', async (req, res) => {
const getInteresPagadoMensualReporteERM = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interes), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo NOT LIKE 'EGRESO%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPagado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/interespagado/mensual', async (req, res) => {
const getInteresPagadoMensualReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interes), 0) as total --+ COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo NOT LIKE 'EGRESO%'; 
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPagadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes pagado mensual:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/ingresosvarios', async (req, res) => {
const getIngresosVariosReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(COALESCE(amortizacion, 0) + COALESCE(interes, 0)), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo LIKE 'INGRESO%' AND idsocio != '02'
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[INGRESOS VARIOS] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total}`);

        res.json({ ingresosVarios: parseFloat(result.rows[0].total) });
    } catch (error) {
        console.error("Error en reporte ingresos varios:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/gastosvarios', async (req, res) => {
const getGastosVariosReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(subconsulta.total), 0) as total_general
            FROM (
                -- Total de Egresos (Gastos/Multas)
                SELECT SUM(COALESCE(amortizacion, 0) + COALESCE(interes, 0)) as total 
                FROM pagos 
                WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
                AND tipo LIKE 'EGRESO%' AND idsocio = '02'

                UNION ALL

                -- Total de Préstamos Entregados (Ayudas)
                SELECT SUM(montoprestado) as total 
                FROM prestamos 
                WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
                AND tipo LIKE 'AYUDA%'
            ) as subconsulta;
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[GASTOS VARIOS] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total_general}`);

        res.json({ egresosSocios: parseFloat(result.rows[0].total_general) });
    } catch (error) {
        console.error("Error en reporte gastos varios:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/interesanulado', async (req, res) => {
const getInteresAnuladoReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(COALESCE(interes, 0)), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo LIKE 'EGRESO%' AND idsocio != '02';
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[INTERÉS ANULADO] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total}`);

        res.json({ egresoInteresAnulado: parseFloat(result.rows[0].total) });
    } catch (error) {
        console.error("Error en reporte intereses anulado:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/aporteindividual', async (req, res) => {
const getAporteIndividualReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(
                SUM(COALESCE(aportado, 0)) / NULLIF((SELECT COUNT(*) FROM socios WHERE idsocio NOT IN ('01', '02')), 0), 
                0
            ) as total_promedio
            FROM aportes
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
            AND idsocio NOT IN ('01', '02')
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[APORTE INDIVIDUAL] Rango: ${inicio} a ${fin} | Promedio: ${result.rows[0].total_promedio}`);

        // IMPORTANTE: Aquí cambié el nombre a 'total_promedio' para que coincida con tu SQL
        res.json({ aporteIndividual: parseFloat(result.rows[0].total_promedio) });
    } catch (error) {
        console.error("Error en reporte aporte individual:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/dinerocaja', async (req, res) => {
const getDineroEnCajaReporteERG = async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(COALESCE(efectivo, 0)), 0) as total 
            FROM caja 
            WHERE fechacaja::date >= $1::date AND fechacaja::date <= $2::date;
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[DINERO CAJA] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total}`);

        res.json({ dineroEnCaja: parseFloat(result.rows[0].total) });
    } catch (error) {
        console.error("Error en reporte dinero en caja:", error.message);
        res.status(500).json({ error: error.message });
    }
};

// app.get('/api/reportes/caja-anterior', async (req, res) => {
const getCajaAnteriorReporteERM = async (req, res) => {
    const { inicio, fin } = req.query;

    try {
        // 1. Convertimos las fechas recibidas a objetos Date
        let fechaInicioPrevia = new Date(inicio);
        let fechaFinPrevia = new Date(fin);

        // 2. Restamos 1 mes a cada fecha para obtener el periodo anterior
        fechaInicioPrevia.setMonth(fechaInicioPrevia.getMonth() - 1);
        fechaFinPrevia.setMonth(fechaFinPrevia.getMonth() - 1);

        // 3. Consulta SQL sumando la caja en ese nuevo rango
        const query = `
            SELECT COALESCE(SUM(efectivo), 0) as total 
            FROM caja 
            WHERE fechacaja::date >= $1::date 
              AND fechacaja::date <= $2::date
        `;

        const result = await pool.query(query, [
            fechaInicioPrevia.toISOString().split('T')[0],
            fechaFinPrevia.toISOString().split('T')[0]
        ]);

        const totalPrevio = parseFloat(result.rows[0].total);

        console.log(`[CAJA ANTERIOR] Rango: ${fechaInicioPrevia.toLocaleDateString()} a ${fechaFinPrevia.toLocaleDateString()} | Total: ${totalPrevio}`);

        res.json({ dineroCajaAnterior: totalPrevio });

    } catch (error) {
        console.error("Error en caja anterior:", error.message);
        res.status(500).json({ error: error.message });
    }
};

module.exports = {
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
};