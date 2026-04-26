const pool = require('../database/connection');

//**********************************SERVICIOS PARA REPORTES***************************************
async function actualizarTodoElSocio(idsocio) {
    try {
        // 1. Sumas de Aportes
        const aporte = await pool.query('SELECT SUM(aportado) as total FROM aportes WHERE idsocio=$1', [idsocio]);

        // 2. Sumas de Préstamos (Filtrando por la columna 'tipo')
        // Usamos "=" porque idsocio es numérico y comparamos 'tipo' para excluir AYUDA
        const prestamo = await pool.query(`
            SELECT SUM(montoprestado) as m, SUM(interestotal) as i 
            FROM prestamos 
            WHERE idsocio = $1 AND tipo = 'PRESTAMO'`, [idsocio]);

        // 3. Cálculo de pagos usando la columna 'tipo'
        // Eliminamos los "LIKE" sobre los IDs numéricos
        const pagos = await pool.query(`
            SELECT 
                SUM(CASE WHEN tipo = 'PAGO' AND numamortizacion != 'GST' THEN amortizacion ELSE 0 END) as cap_pag,
                SUM(CASE WHEN numamortizacion = 'GST' THEN amortizacion ELSE 0 END) as cap_anu,
                SUM(CASE WHEN tipo = 'PAGO' AND numinteres != 'GST' THEN interes ELSE 0 END) as int_pag,
                SUM(CASE WHEN numinteres = 'GST' THEN interes ELSE 0 END) as int_anu
            FROM pagos WHERE idsocio = $1`, [idsocio]);

        // 4. UPDATE físico en la tabla SOCIOS
        const queryUpdate = `
            UPDATE socios SET 
                aportado = $1, montoprestado = $2, montopagado = $3,
                montopendiente = (CAST($2 AS NUMERIC) - (CAST($3 AS NUMERIC) + CAST($4 AS NUMERIC))),
                interesprestado = $5, interespagado = $6, interesanulado = $7,
                interespendiente = (CAST($5 AS NUMERIC) - (CAST($6 AS NUMERIC) + CAST($7 AS NUMERIC)))
            WHERE idsocio = $8`;

        await pool.query(queryUpdate, [
            Number(aporte.rows[0].total || 0),
            Number(prestamo.rows[0].m || 0),
            Number(pagos.rows[0].cap_pag || 0),
            Number(pagos.rows[0].cap_anu || 0),
            Number(prestamo.rows[0].i || 0),
            Number(pagos.rows[0].int_pag || 0),
            Number(pagos.rows[0].int_anu || 0),
            idsocio
        ]);

        console.log(`Datos físicos actualizados para el socio: ${idsocio}`);
    } catch (error) {
        // Esto evitará el error "integer !~~ unknown"
        console.error("Error en actualizarTodoElSocio:", error.message);
    }
}

module.exports = { actualizarTodoElSocio };



